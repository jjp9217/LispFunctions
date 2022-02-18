;author: Jesse Pingitore
; Determines if a statement is true.
; param a: Question
; param b: List of truths
; return: Is this question true?
(defun query (q truth)
    ;have we exhausted the truths yet?
    (if (null truth)

        ;yes, return nil
        nil

        ;no, see if the q and car match
        (if (matchp q (car truth)) ;does it match?
            ;yes, return the matched truth
            (car truth)

            ;no, recurse
            (query q (cdr truth))
        )
    )
)

; A function to check if a two LISTS are equal in all positions.
; The universal symbol *var* will match anything in it's position.
; parem a: list a
; parem b: list b
; return: Bool, do these match?
(defun matchp (a b)
    ;if both atom
    (if (and (atom a) (atom b))
        ;check if they match
        (match-atoms a b)

        ;else, these are lists, match the cars and recurse on cdrs
        (and (match-atoms (car a) (car b)) (matchp (cdr a) (cdr b)))
    )
)

; A function to check if two atoms are equal, or if 
; one matches the universal symbol *var*.
; parem a: atom a
; parem b: atom b
; return: Bool, do these match?
(defun match-atoms (a b)
    (if ( or
            (equal a b)
            (or
                (equal a '*var*)
                (equal b '*var*)
            )
        )
        T
        nil
    )
)