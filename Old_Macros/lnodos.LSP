;mide la longitud de todas las lineas capturadas
(defun c:lml( / sel n i nomi enti a10 a11 di sdi)
(setq sel (ssget '((0 . "LINE")))
n (sslength sel)
i 0
sdi 0
)
(repeat n
(setq nomi (ssname sel i)
enti (entget nomi)
a10 (cdr (assoc 10 enti))
a11 (cdr (assoc 11 enti))
di (distance a10 a11)
sdi (+ sdi di)
i (1+ i)
)
)
(princ "longitud total = ")
(prin1 sdi)
(princ)
) 