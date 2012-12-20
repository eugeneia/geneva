((:S ("A " (:I "real") " Document")
  ((:P
    ("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."))
   (:S ("Text")
    ((:P
      ("Rich text looks like that: " (:B "bold") ", " (:I "italic") ", "
       (:C "code") ", " (:U "http://www.domain/")))))
   (:S ("Objects")
    ((:L
      (("Foo")
       ("Bar baz boo way too long so it breaks the line whooooo lol lolo!! Hope this in enough.")
       ("Baz")))
     (:M ("Media object.") "http://domain/media")
     (:T ("A table, with text")
      ((("foo") ("bar") ("baz"))
       (((:B "foobar")) ((:I "barfoo?")) ((:U "http://triakis.ath.cx/")))))
     (:R ("Some code.") "(defun square (x)
  (* x x))"))))))