;author: Jesse Pingitore
; Count the number of cells in the expression. An expression
;   has one cell per non-nil member, and each of it's members'
;   members if they are lists.
; param expr: The expression to find the number of cells of
; return: The number of cells
(defun count-cells (expr)
    (cond  
        ;if atom
        ((atom expr)
            0
        )

        ;if list
        ((listp expr)
            
            ;if car is atom
            (if (atom (car expr))
                ; +1 and explore further TODO length?
                (+ 1 (count-cells (cdr expr)))

                ;else, car is a list
                (cond 
                    
                    ;if 2+ elements left, recurse into both 
                    ((>= (length expr) 2)
                        (+  1 
                            (+ (count-cells (car expr)) 
                                (count-cells (cdr expr)))
                        )
                    )

                    ;if one element left, recurse into it
                    ((= (length expr) 1)
                        (+ 1 (count-cells (car expr)))
                    )

                    ;if nil, return 0
                    (t 0)
                )
            )
        )
    )
)