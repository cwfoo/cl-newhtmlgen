(asdf:defsystem "newhtmlgen"
    :description "Common Lisp HTML generator."
    :version "0.1.0"
    :licence "LLGPL"
    :components ((:file "newhtmlgen"))
    :in-order-to ((asdf:test-op (asdf:test-op "newhtmlgen/test"))))

(asdf:defsystem "newhtmlgen/test"
  :depends-on ("newhtmlgen")
  :components ((:file "test-newhtmlgen"))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call :newhtmlgen '#:run-tests)))
