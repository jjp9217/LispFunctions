; Return the relative complement of the s1, s1^s2.
; NOTE: Sets may contains lists, but not other sets.
; (a (b ,c)) != (a, (c b))
; param s1: The set to subtract from
; param s2: The set to compare against
; return: relative complement of s1, s1^s2.
(defun rel-complement (s1 s2)
    (if (null s2);if s2 is nil, return s1
        s1
        (if (null s1);if s1 is nil, return nil
            nil
            ;else check length
            (if (= 1 (length s1)) ;base case
                (if (member (car s1) s2 :test 'equal)
                    nil
                    s1
                )
                ;this only works for atoms
                (if (member (car s1) s2 :test 'equal)
                    (rel-complement (cdr s1) s2);exclude
                    (cons (car s1) (rel-complement (cdr s1) s2));include
                )
            )  
        )
    )
)