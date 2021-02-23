;; newhtmlgen.lisp
;;
;; copyright (c) 1986-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2014 Franz Inc, Oakland, CA - All rights reserved.
;; copyright (c) 2021 Foo Chuan Wei - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA  02111-1307  USA

(defpackage :newhtmlgen
  (:use :common-lisp)
  (:export #:html
           #:html-print
           #:html-print-subst
           #:html-print-list
           #:html-print-list-subst
           #:html-stream
           #:*html-stream*))

(in-package :newhtmlgen)

;; JSC - Handling binary output hack
;; The problem is that aserve uses the same socket for transferring
;; binary data (e.g. images) and text data (e.g. HTML). This kludge
;; solves this problem at the cost of I/O performance.
(defun write-html-string (string &optional stream &key (start 0) end)
  (if (and stream (equal (stream-element-type stream)
                         '(unsigned-byte 8)))
      (loop :for i :from start :below (or end (length string))
         :do (write-byte (char-code (schar string i)) stream))
;     (loop :for c :across (subseq string start end)
;        :do (write-byte (char-code c) stream))
      (write-string string stream :start start :end (or end (length string)))))

;;; HTML generation.

(defstruct (html-process (:type list)
                         (:constructor
                           make-html-process
                           (key has-inverse macro special print name-attr)))
  key          ; Keyword naming this tag.
  has-inverse  ; T if the / form is used.
  macro        ; The macro to define this.
  special      ; If true then call this to process the keyword and return
               ; the macroexpansion.
  print        ; Function used to handle this in html-print.
  name-attr)   ; Attribute symbols which can name this object for subst purposes.

(defparameter *html-process-table*
  ;; #'eq is accurate but want to avoid rehashes.
  (make-hash-table :test #'equal))

(defvar *html-stream* nil)  ;; Where the output goes.

(defmacro html-out-stream-check (stream)
  ;; Ensure that a real stream is passed to this function.
  `(let ((.str. ,stream))
     (when (not (streamp .str.))
       (error "html-stream must be passed a stream object, not ~s" .str.))
     .str.))

(defmacro html-stream (stream &rest forms)
  ;; Set output stream and emit HTML.
  `(let ((*html-stream* (html-out-stream-check ,stream))) (html ,@forms)))

(defun process-html-forms (forms env)
  (let (res)
    (flet ((do-ent (ent args argsp body)
             ;; ent is an html-process object associated with the HTML tag
             ;;   we're processing.
             ;; args is the list of values after the tag in the form
             ;;   ((:tag &rest args) ....).
             ;; argsp is true if this isn't a singleton tag (i.e. it has a body)
             ;;   .. (:tag ...) or ((:tag ...) ...).
             ;; body is the body if any of the form.
             (let (spec)
               (cond ((setq spec (html-process-special ent))
                      ;; Do something different.
                      (push (funcall spec ent args argsp body) res))
                     ((null argsp)
                      ;; Singleton tag, just do the set.
                      (push `(,(html-process-macro ent) :set) res)
                      nil)
                     (t (cond ((equal args '(:unset))
                               ;; ((:tag :unset)) is a special case that allows
                               ;; us to close off singleton tags printed earlier.
                               (push `(,(html-process-macro ent) :unset) res)
                               nil)
                              (t  ; Some args.
                               (push `(,(html-process-macro ent) ,args
                                        ,(process-html-forms body env))
                                     res)
                               nil)))))))

      (do* ((xforms forms (cdr xforms))
            (form (car xforms) (car xforms)))
           ((null xforms))

        (setq form (macroexpand form env #+(and allegro (version>= 7 0)) t))

        (cond ((atom form)
               (cond ((keywordp form)
                      (let ((ent (gethash form *html-process-table*)))
                        (if (null ent)
                            (error "unknown html keyword ~s" form)
                            (do-ent ent nil nil nil))))
                     ((stringp form)
                      ;; Turn into a print of it.
                      (push `(write-html-string ,form *html-stream*) res))
                     (t (push form res))))
              (t
               (let ((first (car form)))
                 (cond ((keywordp first)
                        ;; (:xxx . body) form.
                        (let ((ent (gethash first
                                            *html-process-table*)))
                          (if (null ent)
                              (error "unknown html keyword ~s" form)
                              (do-ent ent nil t (cdr form)))))
                       ((and (consp first) (keywordp (car first)))
                        ;; ((:xxx args ) . body).
                        (let ((ent (gethash (car first)
                                            *html-process-table*)))
                          (if (null ent)
                              (error "unknown html keyword ~s" form)
                              (do-ent ent (cdr first) t (cdr form)))))
                       (t (push form res))))))))
    `(progn ,@(nreverse res))))

(defmacro html (&body forms &environment env)
  ;; Just emit HTML to the current stream.
  (process-html-forms forms env))

(defun html-atom-check (args open close body)
  (when (and args (atom args))
    (let ((ans (case args
                 (:set `(write-html-string ,open *html-stream*))
                 (:unset `(write-html-string ,close *html-stream*))
                 (t (error "illegal arg ~s to ~s" args open)))))
      (if (and ans body)
          (error "can't have a body form with this arg: ~s" args)
          ans))))

(defun html-body-form (open close body)
  ;; Used when args don't matter.
  `(progn (write-html-string ,open *html-stream*)
          ,@body
          (write-html-string ,close *html-stream*)))

(defun html-body-key-form (string-code has-inv args body)
  ;; Do what's needed to handle given keywords in the args then do the body.
  (when (and args (atom args))
    ;; Single arg.
    (return-from html-body-key-form
      (case args
        (:set `(write-html-string ,(format nil "<~a>" string-code)
                                  *html-stream*))
        (:unset (when has-inv
                  `(write-html-string ,(format nil "</~a>" string-code)
                                      *html-stream*)))
        (t (error "illegal arg ~s to ~s" args string-code)))))

  (when (not (evenp (length args)))
    (warn "arg list ~s isn't even" args))

  (cond (args
         `(progn (write-string ,(format nil "<~a" string-code)
                               *html-stream*)
                 ,@(do ((xx args (cddr xx))
                        (res))
                       ((null xx)
                        (nreverse res))
                     (cond ((eq :if* (car xx))
                            ;; Insert following conditionally.
                            (push `(when ,(cadr xx)
                                     (write-string ,(format nil " ~a" (caddr xx))
                                                   *html-stream*)
                                     (prin1-safe-http-string ,(cadddr xx)))
                                  res)
                            (pop xx)
                            (pop xx))
                           (t
                            (push `(write-string
                                    ,(format nil " ~a" (car xx))
                                    *html-stream*)
                                  res)
                            (push `(prin1-safe-http-string ,(cadr xx)) res))))

                 (write-string ">" *html-stream*)

                 ,@body
                 ,(when (and body has-inv)
                    `(write-string ,(format nil "</~a>" string-code)
                                   *html-stream*))))
        (t `(progn (write-string ,(format nil "<~a>" string-code)
                                 *html-stream*)
                   ,@body
                   ,(when (and body has-inv)
                      `(write-string ,(format nil "</~a>" string-code)
                                     *html-stream*))))))

(defun princ-http (val)
  ;; Print the given value to the HTML stream using ~a.
  (format *html-stream* "~a" val))

(defun prin1-http (val)
  ;; Print the given value to the HTML stream using ~s.
  (format *html-stream* "~s" val))

(defun emit-safe (stream string)
  ;; Send the string to the HTTP response stream watching out for special HTML
  ;; characters and encoding them appropriately.
  (do* ((i 0 (1+ i))
        (start i)
        (end (length string)))
       ((>= i end)
        (when (< start i)
          (write-html-string string
                             stream
                             :start start
                             :end i)))
    (let ((ch (schar string i))
          (cvt ))
      (cond ((eql ch #\<)
             (setq cvt "&lt;"))
            ((eq ch #\>)
             (setq cvt "&gt;"))
            ((eq ch #\&)
             (setq cvt "&amp;"))
            ((eq ch #\")
             (setq cvt "&quot;")))
      (when cvt
        ;; Must do a conversion, emit previous chars first.
        (when (< start i)
          (write-sequence string
                          stream
                          :start start
                          :end i))
        (write-string cvt stream)
        (setq start (1+ i))))))

(defun princ-safe-http (val)
  (emit-safe *html-stream* (format nil "~a" val)))

(defun prin1-safe-http (val)
  (emit-safe *html-stream* (format nil "~s" val)))

(defun prin1-safe-http-string (val)
  ;; Used only in a parameter value situation.
  ;;
  ;; If the parameter value is the symbol with the empty print name, then turn
  ;; this into a singleton object. Thus || is different than "".
  ;;
  ;; Print the contents inside a string double quotes (which should not be
  ;; turned into &quot;'s.
  ;; Symbols are turned into their name.
  (cond ((and (symbolp val)
              (equal "" (symbol-name val)))
         nil)  ; Do nothing.
        (t
         (write-char #\= *html-stream*)
         (cond ((or (stringp val)
                    (and (symbolp val)
                         (setq val (symbol-name val))))
                (write-char #\" *html-stream*)
                (emit-safe *html-stream* val)
                (write-char #\" *html-stream*))
               (t
                (prin1-safe-http val))))))

(defun html-find-value (key subst)
  ;; Find the (key . value) object in the subst list.
  ;; A subst list is an assoc list ((key . value) ....) but instead of a
  ;; (key . value) cons you may have an assoc list.
  (let ((to-process nil)
        (alist subst))
    (loop
       (do* ((entlist alist (cdr entlist))
             (ent (car entlist) (car entlist)))
            ((null entlist) (setq alist nil))
         (cond ((consp (car ent))
                ;; This is another alist.
                (when (cdr entlist)
                  (push (cdr entlist) to-process))
                (setq alist ent)
                (return))  ;; Exit do*.
               ((equal key (car ent))
                (return-from html-find-value ent))))
       (when (null alist)
         ;; We need to find a new alist to process.
         (if to-process
             (setq alist (pop to-process))
             (return))))))

(defun html-print-subst (form subst stream unknown)
  ;; Print the given lhtml form to the given stream.
  (assert (streamp stream))
  (let* ((attrs)
         (attr-name)
         (name)
         (possible-kwd (cond ((atom form)
                              form)
                             ((consp (car form))
                              (setq attrs (cdar form))
                              (caar form))
                             (t (car form))))
         print-handler
         ent)
    (when (keywordp possible-kwd)
      (cond ((null (setq ent (gethash possible-kwd *html-process-table*)))
             (if unknown
                 (return-from html-print-subst (funcall unknown form stream))
                 (error "unknown html tag: ~s" possible-kwd)))
            (t
             ;; See if we should subst.
             (when (and subst
                        attrs
                        (setq attr-name (html-process-name-attr ent))
                        (setq name (getf attrs attr-name))
                        (setq attrs (html-find-value name subst)))
               (return-from html-print-subst
                 (if (functionp (cdr attrs))
                     (funcall (cdr attrs) stream)
                     (html-print-subst (cdr attrs)
                                       subst
                                       stream
                                       unknown))))))
      (setq print-handler
            (html-process-print ent)))

    (cond ((atom form)
           (cond ((keywordp form)
                  (funcall print-handler ent :set nil nil nil nil stream))
                 ((stringp form)
                  (write-string form stream))
                 (t (princ form stream))))
          (ent
           (funcall print-handler
                    ent
                    :full
                    (when (consp (car form)) (cdr (car form)))
                    form
                    subst
                    unknown
                    stream))
          (t (error "Illegal form: ~s" form)))))

(defun html-print-list (list-of-forms stream &key unknown)
  ;; HTML print a list of forms.
  (dolist (x list-of-forms)
    (html-print-subst x nil stream unknown)))

(defun html-print-list-subst (list-of-forms subst stream &key unknown)
  ;; HTML print a list of forms.
  (dolist (x list-of-forms)
    (html-print-subst x subst stream unknown)))

(defun html-print (form stream &key unknown)
  (html-print-subst form nil stream unknown))

(defun html-standard-print (ent cmd args form subst unknown stream)
  ;; The print handler for the normal HTML operators.
  (ecase cmd
    (:set  ; Just turn it on.
     (format stream "<~a>" (html-process-key ent)))
    (:full  ; Set, do body and then unset.
     (let (iter)
       (if args
           (cond ((and (setq iter (getf args :iter))
                       (setq iter (html-find-value iter subst)))
                  ;; Remove the iter and pre.
                  (setq args (copy-list args))
                  (remf args :iter)
                  (funcall (cdr iter)
                           (cons (cons (caar form)
                                       args)
                                 (cdr form))
                           subst
                           stream)
                  (return-from html-standard-print))
                 (t
                  (format stream "<~a" (html-process-key ent))
                  (do ((xx args (cddr xx)))
                      ((null xx))
                    ;; Assume that the arg is already escaped since we read it
                    ;; from the parser.
                    (format stream " ~a=\"~a\"" (car xx) (cadr xx)))
                  (format stream ">")))
           (format stream "<~a>" (html-process-key ent)))
       (dolist (ff (cdr form))
         (html-print-subst ff subst stream unknown)))
     (when (html-process-has-inverse ent)
       ;; End the form.
       (write-html-string (format nil "</~a>" (html-process-key ent)) stream)))))

;;; -- Defining how html tags are handled. --
;;;
;;; Most tags are handled in a standard way and the def-std-html macro is used
;;; to define such tags.
;;;
;;; Some tags need special treatment and def-special-html defines how these are
;;; handled. The tags requiring special treatment are the pseudo tags we added
;;; to control operations in the HTML generator.
;;;
;;; Tags can be found in three ways:
;;; :br - singleton, no attributes, no body.
;;; (:b "foo") - no attributes but with a body.
;;; ((:a href "foo") "balh") - attributes and body.

(defmacro def-special-html (kwd fcn print-fcn)
  ;; kwd - The tag we're defining behavior for.
  ;; fcn - Function to compute the macroexpansion of a use of this tag.
  ;;     Args to fcn are:
  ;;       ent - html-process object holding info on this tag.
  ;;       args - List of attribute-values following tag.
  ;;       argsp - True if there is a body in this use of the tag.
  ;;       body - List of body forms.
  ;; print-fcn - Function to print an lhtml form with this tag.
  ;;     Args to print-fcn are:
  ;;       ent - html-process object holding info on this tag.
  ;;       cmd - One of :set, :unset, :full.
  ;;       args - List of attribute-value pairs.
  ;;       subst - Substitution list.
  ;;       unknown - Function to call for unknown tags.
  ;;       stream - Stream to write to.
  `(setf (gethash ,kwd *html-process-table*)
         (make-html-process ,kwd nil nil ,fcn ,print-fcn nil)))

(def-special-html :newline
  #'(lambda (ent args argsp body)
      (declare (ignore ent args argsp))
      (when body
        (error "can't have a body with :newline -- body is ~s" body))
      `(terpri *html-stream*))
  #'(lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (if (eq cmd :set)
        (terpri stream)
        (error ":newline in an illegal place: ~s" form))))

(def-special-html :princ
  #'(lambda (ent args argsp body)
      (declare (ignore ent args argsp))
      `(progn ,@(mapcar #'(lambda (bod)
                            `(princ-http ,bod))
                        body)))
  #'(lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent subst unknown))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (write-html-string (format nil "~a" (cadr form)) stream)
          (error ":princ must be given an argument"))))

(def-special-html :princ-safe
  #'(lambda (ent args argsp body)
      (declare (ignore ent args argsp))
      `(progn ,@(mapcar #'(lambda (bod)
                            `(princ-safe-http ,bod))
                        body)))
  #'(lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent unknown subst))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (emit-safe stream (format nil "~a" (cadr form)))
          (error ":princ-safe must be given an argument"))))

(def-special-html :prin1
  #'(lambda (ent args argsp body)
      (declare (ignore ent args argsp))
      `(progn ,@(mapcar #'(lambda (bod)
                            `(prin1-http ,bod))
                        body)))
  #'(lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent args unknown subst))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (format stream "~s" (cadr form))
          (error ":prin1 must be given an argument"))))

(def-special-html :prin1-safe
  #'(lambda (ent args argsp body)
      (declare (ignore ent args argsp))
      `(progn ,@(mapcar #'(lambda (bod)
                            `(prin1-safe-http ,bod))
                        body)))
  #'(lambda (ent cmd args form subst unknown stream)
      (declare (ignore args ent subst unknown))
      (assert (eql 2 (length form)))
      (if (eq cmd :full)
          (emit-safe stream (format nil "~s" (cadr form)))
          (error ":prin1-safe must be given an argument"))))

(def-special-html :comment
  #'(lambda (ent args argsp body)
      ;; Must use <!-- --> syntax.
      (declare (ignore ent args argsp))
      `(progn (write-html-string "<!--" *html-stream*)
              (html ,@body)
              (write-html-string "-->" *html-stream*)))
  #'(lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent cmd args subst unknown))
      (write-html-string (format nil "<!--~a-->" (cadr form)) stream)))

(defmacro def-std-html (kwd has-inverse name-attrs)
  (let ((mac-name (intern (format nil "~a-~a" :with-html kwd)))
        (string-code (string-downcase (string kwd))))
    `(progn (setf (gethash ,kwd *html-process-table*)
                  (make-html-process ,kwd ,has-inverse
                                     ',mac-name
                                     nil
                                     #'html-standard-print
                                     ',name-attrs))
            (defmacro ,mac-name (args &rest body)
              (html-body-key-form ,string-code ,has-inverse args body)))))

(def-std-html :a t nil)
(def-std-html :abbr t nil)
(def-std-html :acronym t nil)
(def-std-html :address t nil)
(def-std-html :applet t nil)
(def-std-html :area nil nil)

(def-std-html :b t nil)
(def-std-html :base nil nil)
(def-std-html :basefont nil nil)
(def-std-html :bdo t nil)
(def-std-html :bgsound nil nil)
(def-std-html :big t nil)
(def-std-html :blink t nil)
(def-std-html :blockquote t nil)
(def-std-html :body t nil)
(def-std-html :br nil nil)
(def-std-html :button t nil)

(def-std-html :caption t nil)
(def-std-html :center t nil)
(def-std-html :cite t nil)
(def-std-html :code t nil)
(def-std-html :col nil nil)
(def-std-html :colgroup nil nil)

(def-std-html :dd t nil)
(def-std-html :del t nil)
(def-std-html :dfn t nil)
(def-std-html :dir t nil)
(def-std-html :div t nil)
(def-std-html :dl t nil)
(def-std-html :dt t nil)

(def-std-html :em t nil)

(def-std-html :fieldset t nil)
(def-std-html :font t nil)
(def-std-html :form t :name)
(def-std-html :frame t nil)
(def-std-html :frameset t nil)

(def-std-html :h1 t nil)
(def-std-html :h2 t nil)
(def-std-html :h3 t nil)
(def-std-html :h4 t nil)
(def-std-html :h5 t nil)
(def-std-html :h6 t nil)
(def-std-html :head t nil)
(def-std-html :hr nil nil)
(def-std-html :html t nil)

(def-std-html :i t nil)
(def-std-html :iframe t nil)
(def-std-html :ilayer t nil)
(def-std-html :img nil :id)
(def-std-html :input nil nil)
(def-std-html :ins t nil)
(def-std-html :isindex nil nil)

(def-std-html :kbd t nil)

(def-std-html :label t nil)
(def-std-html :layer t nil)
(def-std-html :legend t nil)
(def-std-html :li t nil)
(def-std-html :link nil nil)
(def-std-html :listing t nil)

(def-std-html :map t nil)
(def-std-html :marquee t nil)
(def-std-html :menu t nil)
(def-std-html :meta nil nil)
(def-std-html :multicol t nil)

(def-std-html :nobr t nil)
(def-std-html :noembed t nil)
(def-std-html :noframes t nil)
(def-std-html :noscript t nil)

(def-std-html :object t nil)
(def-std-html :ol t nil)
(def-std-html :optgroup t nil)
(def-std-html :option t nil)

(def-std-html :p t nil)
(def-std-html :param t nil)
(def-std-html :plaintext nil nil)
(def-std-html :pre t nil)

(def-std-html :q t nil)

(def-std-html :s t nil)
(def-std-html :samp t nil)
(def-std-html :script t nil)
(def-std-html :select t nil)
(def-std-html :server t nil)
(def-std-html :small t nil)
(def-std-html :spacer nil nil)
(def-std-html :span t :id)
(def-std-html :strike t nil)
(def-std-html :strong t nil)
(def-std-html :style t nil)
(def-std-html :sub t nil)
(def-std-html :sup t nil)

(def-std-html :table t :name)
(def-std-html :tbody t nil)
(def-std-html :td t nil)
(def-std-html :textarea t nil)
(def-std-html :tfoot t nil)
(def-std-html :th t nil)
(def-std-html :thead t nil)
(def-std-html :title t nil)
(def-std-html :tr t nil)
(def-std-html :tt t nil)

(def-std-html :u t nil)
(def-std-html :ul t nil)

(def-std-html :var t nil)

(def-std-html :xmp t nil)

;;; HTML5.

(def-std-html :section t nil)
(def-std-html :article t nil)
(def-std-html :main t nil)
(def-std-html :aside t nil)
(def-std-html :hgroup t nil)
(def-std-html :header t nil)
(def-std-html :footer t nil)
(def-std-html :nav t nil)
(def-std-html :figure t nil)
(def-std-html :figcaption t nil)

(def-std-html :video t nil)
(def-std-html :audio t nil)
(def-std-html :source t nil)
(def-std-html :track t nil)
(def-std-html :embed t nil)
(def-std-html :mark t nil)
(def-std-html :progress t nil)
(def-std-html :meter t nil)
(def-std-html :time t nil)
(def-std-html :data t nil)
(def-std-html :dialog t nil)
(def-std-html :ruby t nil)
(def-std-html :rt t nil)
(def-std-html :rp t nil)
(def-std-html :bdi t nil)
(def-std-html :wbr nil nil)
(def-std-html :canvas t nil)
(def-std-html :menuitem t nil)
(def-std-html :details t nil)
(def-std-html :datalist t nil)
(def-std-html :keygen nil nil)
(def-std-html :output t nil)
