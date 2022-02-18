;author: Jesse Pingitore
; Count the number of times that a pattern is matched
;   within an expression.
; param ele: The pattern to check within the expression
; param expr: The expression to match against
; return: the number of times the pattern is matched
(defun count-anywhere (ele expr)
    ;are there still memebers to check?
    (if (>= (length expr) 1)
        ;switch for types
        (cond 
            ;if both list
            ( (and (listp ele) (listp (car expr)))
                (if (equal ele (car expr)) 
                    ;lisp match, increment and continue to check lst
                    (+ 1 (count-anywhere ele (cdr expr)))
                    ;not equal, continue searching the car AND cdr
                    (+ (count-anywhere ele (car expr))
                        (count-anywhere ele (cdr expr))
                    )
                )

            )

            ;if both atom
            ( (and (atom ele) (atom (car expr)))
                (if (equal ele (car expr)) 
                    ;atom match, increment and continue to check
                    (+ 1 (count-anywhere ele (cdr expr)))
                    ;not equal, continue searching cdr
                    (+ 0 (count-anywhere ele (cdr expr)))
                )
            )

            ;if atom and list
            ( (and (atom ele) (list (car expr)))
                ;recurse into the list
                (+ (count-anywhere ele (car expr))
                    (count-anywhere ele (cdr expr))
                    )
            )

            ;if list and atom
            ( (and (list ele) (atom (car expr)))
                ;recurse into the cdr
                (count-anywhere ele (cdr expr))
            )
            ;default
            (t nil)
        );cond end
        ;else end, no more matches anywhere
        0
    )
)