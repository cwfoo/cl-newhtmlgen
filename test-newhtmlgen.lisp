;;;; Unit tests for newhtmlgen.lisp.

(in-package :newhtmlgen)

(defun test-simple-table-a ()
  (let ((result
          (with-output-to-string (p)
            (html-stream p
              (:html
                (:head (:title "Test Table"))
                (:body (:table
                         (:tr (:td "0") (:td "0"))
                         (:tr (:td "1") (:td "1"))
                         (:tr (:td "2") (:td "4"))
                         (:tr (:td "3") (:td "9"))
                         (:tr (:td "4") (:td "16"))
                         (:tr (:td "5") (:td "25"))))))))
        (expected-result
          (format nil
                  "<html>~
                     <head><title>Test Table</title></head>~
                     <body>~
                       <table>~
                         <tr><td>0</td><td>0</td></tr>~
                         <tr><td>1</td><td>1</td></tr>~
                         <tr><td>2</td><td>4</td></tr>~
                         <tr><td>3</td><td>9</td></tr>~
                         <tr><td>4</td><td>16</td></tr>~
                         <tr><td>5</td><td>25</td></tr>~
                       </table>~
                     </body>~
                   </html>")))
    (assert (string-equal result expected-result))))

(defun test-simple-table-b ()
  (let ((result
          (with-output-to-string (p)
            (html-stream p
              (:html
                (:head (:title "Test Table"))
                (:body ((:table border 2)
                        (:tr (:td "0") (:td "0"))
                        (:tr (:td "1") (:td "1"))
                        (:tr (:td "2") (:td "4"))
                        (:tr (:td "3") (:td "9"))
                        (:tr (:td "4") (:td "16"))
                        (:tr (:td "5") (:td "25"))))))))
        (expected-result
          (format nil
                  "<html>~
                     <head><title>Test Table</title></head>~
                     <body>~
                       <table BORDER=2>~
                         <tr><td>0</td><td>0</td></tr>~
                         <tr><td>1</td><td>1</td></tr>~
                         <tr><td>2</td><td>4</td></tr>~
                         <tr><td>3</td><td>9</td></tr>~
                         <tr><td>4</td><td>16</td></tr>~
                         <tr><td>5</td><td>25</td></tr>~
                       </table>~
                     </body>~
                   </html>")))
    (assert (string-equal result expected-result))))

(defun simple-table-c (count)
  (with-output-to-string (p)
    (html-stream p
      (:html
        (:head (:title "Test Table"))
        (:body ((:table border 2)
                (dotimes (i count)
                  (html (:tr (:td (:princ i))
                             (:td (:princ (* i i))))))))))))

(defun test-simple-table-c ()
  (let ((expected-result
          (format nil
                  "<html>~
                     <head><title>Test Table</title></head>~
                     <body>~
                       <table BORDER=2>~
                         <tr><td>0</td><td>0</td></tr>~
                         <tr><td>1</td><td>1</td></tr>~
                         <tr><td>2</td><td>4</td></tr>~
                         <tr><td>3</td><td>9</td></tr>~
                         <tr><td>4</td><td>16</td></tr>~
                       </table>~
                     </body>~
                   </html>")))
    (assert (string-equal (simple-table-c 5)
                          expected-result))))

(defun simple-table-d (count border-width backg-color border-color)
  (with-output-to-string (p)
    (html-stream p
      (:html
        (:head (:title "Test Table"))
        (:body ((:table border border-width
                        bordercolor border-color
                        bgcolor backg-color
                        cellpadding 3)
                (:tr ((:td bgcolor "blue")
                      ((:font :color "white" :size "+1")
                       "Value"))
                     ((:td bgcolor "blue")
                      ((:font :color "white" :size "+1")
                       "Square")))
                (dotimes (i count)
                  (html (:tr (:td (:princ i))
                             (:td (:princ (* i i))))))))))))

(defun test-simple-table-d ()
  (let ((expected-result
          (format nil
                  "<html>~
                     <head><title>Test Table</title></head>~
                     <body>~
                       <table BORDER=2 ~
                              BORDERCOLOR=\"cyan\" ~
                              BGCOLOR=\"honeydew\" ~
                              CELLPADDING=3>~
                         <tr>~
                           <td BGCOLOR=\"blue\">~
                             <font COLOR=\"white\" SIZE=\"+1\">Value</font>~
                           </td>~
                           <td BGCOLOR=\"blue\">~
                             <font COLOR=\"white\" SIZE=\"+1\">Square</font>~
                           </td>~
                         </tr>~
                         <tr><td>0</td><td>0</td></tr>~
                         <tr><td>1</td><td>1</td></tr>~
                         <tr><td>2</td><td>4</td></tr>~
                         <tr><td>3</td><td>9</td></tr>~
                         <tr><td>4</td><td>16</td></tr>~
                       </table>~
                     </body>~
                   </html>")))
    (assert (string-equal (simple-table-d 5 2 "honeydew" "cyan")
                          expected-result))))

(defun run-tests ()
  (test-simple-table-a)
  (test-simple-table-b)
  (test-simple-table-c)
  (test-simple-table-d)
  (format t "All test cases succeeded.~%"))
