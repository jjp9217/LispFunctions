;author: Jesse Pingitore
; Determine the type of all atoms within an expression
; param s-exp: The expression to evaluate
; return: The s-exp with all of the atom values replaced with types
(defun typer (s-exp)
    ;if atomic,

    (if (null s-exp)

        ;don't include a nil
        nil

        ;if it's an atom
        (if (atom s-exp)

            ;determine atom type
            (if (numberp s-exp) 
                'NUMBER ;a number
                'SYMBOL ;a symbol
            )

            ;else it's a list
            ;recombine recursions on both sides
            (cons (typer (car s-exp)) (typer (cdr s-exp)))
            
        )
    )   
)