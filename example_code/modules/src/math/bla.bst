(func $bla
  (if (< u8)
    (add u8)
    (add u8)
    (add u8)))

(func $blah
  (if (< u8)
    (add u8)
    (add u8)
    (add u8)
  (else
    (sub u8)
    (sub u8)
    (sub u8))))

(export $sqrt)
