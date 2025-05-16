#lang racket
(require racket/format)


; Alphabet builder function
(define (build-alphabet size)
  (map (lambda (i) (string (integer->char (+ 65 i)))) (range size)))


; Linguistic sequence builder function
(define (build-linguistic-sequence numbers sorted alphabet)
  (define min_number (first sorted))
  (define max_number (last sorted))
  (define size (length alphabet))
  (define interval (/ (- max_number min_number) size))
  (map (lambda (num)
         (define idx (inexact->exact (ceiling (/ (- num min_number) interval))))
         (list-ref alphabet (cond [(< idx 1) 0]
                                  [(>= idx size) (sub1 size)]
                                  [else (sub1 idx)])))
       numbers))


; Build precedence matrix (vector of vectors)
(define (build-precedence-matrix sequence alphabet)
  (define size (length alphabet))
  (define matrix (build-vector size (lambda (_) (make-vector size 0))))
  (define get-alphabet-index (lambda (val) (index-of alphabet (list-ref sequence val))))

  ; Iterating over the linguistic sequence,
  ; checking the letter precedence and counting the pairs into the matrix
  (for ((i (in-range (sub1 (length sequence)))))
    (let* ([row (get-alphabet-index i)]
           [col (get-alphabet-index (add1 i))]
           [counter (vector-ref (vector-ref matrix row) col)])
      (vector-set! (vector-ref matrix row) col (add1 counter))))
  
  matrix)


; Matrix printer function
(define (print-matrix matrix alphabet)
  (display " ")
  (for-each (lambda (ch) (display (~a ch #:min-width 8 #:align 'right))) alphabet)
  (newline)
  (for ((i (in-range (vector-length matrix))))
    (printf "~a" (list-ref alphabet i))
    (for ((val (in-vector (vector-ref matrix i))))
      (display (~a val #:min-width 8 #:align 'right)))
    (newline)))


; Alphabet size input loop function
(define (alphabet-size-input-loop)
  (let loop ()
    (display "Enter alphabet size: ")
    (flush-output)
    (define input (read))

    ;; Checking the alphabet size
    (if (> input 26)
        (begin (display "Number is too large, try again\n") (loop))
        input)))

; Main program
(define (main)
  ; Reading alphabet size and constructing the alphabet
  (define alphabet-size (alphabet-size-input-loop))
  (define alphabet (build-alphabet alphabet-size))

  ; Reading number sequence from the file and copy-sorting it
  (define filename "data.txt")
  (define numbers (file->list filename))
  (define sorted (sort numbers <))

  ; Building a linguistic sequence and a precedence matrix
  (define linguistic-sequence (build-linguistic-sequence numbers sorted alphabet))
  (define precedence-matrix (build-precedence-matrix linguistic-sequence alphabet))

  ; Outputting the data
  (printf "Alphabet Size: ~a\n" alphabet-size)
  (printf "Alphabet: ~a\n" (string-join alphabet ""))
  (printf "Linguistic Sequence: ~a\n" (string-join linguistic-sequence ""))
  (printf "Precefence Matrix:\n")
  (print-matrix precedence-matrix alphabet))

(main)
