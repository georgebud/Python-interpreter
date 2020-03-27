# Python-interpreter
 minimalist interpreter for python language using functional paradigm

 - În acestă temă s-a aprofundat CPython, prima implementare de Python și cea mai folosită, care creează ceea ce se numește python byte code și mai apoi este executat de ceea ce se numește byterun.
 - Din tot interpretorul a fost implementat doar un byterun minimalist în Racket.

- CPython folosește un model numit stack machine model: pentru a opera instrucțiuni se folosește de o stivă. Pe lângă această stivă, interpretorul are nevoie să țină mai multe dicționare pentru a putea încărca valori imediate:
 - co_varnames = asocierea dintre numele variabilelor și valorile acestora din program care se schimbă la un moment dat în timp
 - co_constants = dicționar cu valorile constante
 - co_names = dicționar cu nume de funcții
