; Is this a set? (Unique elements only)
; param s: List to determine set-iness
; return: Bool, is this a set
(defun setp (s)
    (if (and s (listp s))
        (if (= 1 (length s))
            T ;it's a set!

            ;else, there is 2+ elements, need to test
            
            (if (member (car s) (cdr s) :test 'equal)
                ;if the first el is a member in the rest

                nil ;false, will return false

                ;else, recurse with the cdr
                (setp (cdr s))
            )
        )
        ;else, s is nil. we need to catch it or it explodes
        nil
    )
)