; Function to insert an element into a setp
; param ele: the element to insert
; param s: the set. may be given as atom or nil,
;   it will be converted into a list 
; return: a (copied) version of s with ele included
(defun insert (ele s)

    ;make sure s isn't an atom
    (if (listp s)

        ;if ele is not nil, check if member
        (if ele

            ;if the element is already in s
            (if (member ele s :test 'equal)

                ;return s plainly
                s

                ;else, add it to the front
                (cons ele s);return a concat'd version of s
            )

            ;else ele is nil, return s
            s
        )

        ;else s is atom (not nil), convert to lst
        (insert ele (list s));and do this again
    )
)