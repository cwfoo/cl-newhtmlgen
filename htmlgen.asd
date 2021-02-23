(defsystem htmlgen
    :author "John K. Foderaro"
    :licence "LLGPL"
    :components ((:file "htmlgen"))
    :perform (load-op :after (op htmlgen)
                      (pushnew :htmlgen cl:*features*)))
