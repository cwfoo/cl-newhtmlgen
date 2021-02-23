(asdf:defsystem "newhtmlgen"
    :description "Common Lisp HTML generator."
    :version "0.1.0"
    :licence "LLGPL"
    :components ((:file "newhtmlgen"))
    :perform (load-op :after (op newhtmlgen)
                      (pushnew :newhtmlgen cl:*features*)))
