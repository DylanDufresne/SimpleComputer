(#%require (only racket read-line))
(#%require (only racket delete-file))
(#%require (only racket string-contains?))
(#%require (only racket file-exists?))

(define make-FileList
  (lambda (fileName)
  (let ((file (open-input-file fileName)))
    (let fileRead ((line (read-line file)))
      (if (eof-object? line)
          (begin
            (close-input-port file)
            '())
          (cons line (fileRead (read-line file))))))))

(define getIndexOfChar
  (lambda (string char index)
      (if (= (string-length string) 0)
          index
          (if (equal? (substring string 0 1) char)
              index
              (getIndexOfChar (substring string 1 (string-length string)) char (+ index 1))))))

(define commandList '(("STOP" . 0000) ("LD" . 100) ("STO" . 200) ("ADD" . 300) ("SUB" . 400) ("MPY" . 500)
                                   ("DIV" . 600) ("IN" . 700) ("OUT" . 800) ("BR" . 900) ("BZ" . 1000) ("BGTR" . 1100)))

(define make-Compiler
  (lambda (fileIn fileOut)
    (let ((locationCounter 00)
          (availableMemory 99)
          (symbolList '())
          (dataSymbolList '())
          (commands commandList))
      (lambda (msg)
        (case msg
          ((firstPass)
           (if (file-exists? "temp.txt")
               (delete-file "temp.txt"))
           (let ((inputList (make-FileList fileIn)))
             (do ((i 0 (+ i 1))
                  (fileLength (length inputList))
                  (inputList inputList (cdr inputList))
                  (file (open-output-file "temp.txt")))
               ((<= fileLength i) (close-output-port file))
               (begin
                 ;check the first three letters of the entry, if it is not REM, continue to check for labels
                 (if (not (equal? "REM" (substring (car inputList) 0 3)))
                     (if (string-contains? (car inputList) ":")
                         (if (string-contains? (car inputList) "DC")
                             (begin
                               (set! symbolList (cons (cons (substring (car inputList) 0 (getIndexOfChar (car inputList) ":" 0)) availableMemory) symbolList)) ;(X . 99)
                               
                               (set! dataSymbolList (cons (cons availableMemory (substring (car inputList) (+ (getIndexOfChar (car inputList) ":" 0) 5) (string-length (car inputList)))) dataSymbolList)) ;(99 . 1)
                              (set! availableMemory (- availableMemory 1)))
                             (begin
                               (set! symbolList (cons (cons (substring (car inputList) 0 (getIndexOfChar (car inputList) ":" 0)) locationCounter) symbolList))
                               (set! locationCounter (+ locationCounter 1))
                               (display (substring (car inputList) (+ (getIndexOfChar (car inputList) ":" 0) 2) (string-length (car inputList))) file)
                               (newline file)))
                         (begin
                           (display (car inputList) file)
                           (newline file)
                           (set! locationCounter (+ locationCounter 1)))
                         ))
                 
                 
                 
                 ))
             ))
          ((secondPass)
           (let ((inputList (make-FileList "temp.txt")))
             (if (file-exists? fileOut)
                 (delete-file fileOut))
             (delete-file "temp.txt")
             (do ((i 0 (+ i 1))
                  (fileLength (length inputList))
                  (inputList inputList (cdr inputList))
                  (file (open-output-file fileOut)))
               ((<= fileLength i) (do ((j locationCounter (+ j 1)))
                                    ((>= j (+ availableMemory 1)) (do ((k (+ availableMemory 1) (+ k 1)))
                                                              ((>= k 100) (close-output-port file))
                                                              (begin
                                                                (display (cdr (assoc k dataSymbolList)) file)
                                                                    (newline file))
                                                              ))
                                    (begin
                                      (display "0000" file)
                                      (newline file))
                                    ))
               (begin
                 (if (string-contains? (car inputList) "STOP")
                     (display "0000" file)
                     (begin
                       (display (+ (cdr (assoc (substring (car inputList) 0 (getIndexOfChar (car inputList) " " 0)) commands)) (cdr (assoc (substring (car inputList) (+ (getIndexOfChar (car inputList) " " 0) 1) (- (string-length (car inputList)) 1)) symbolList))) file)
                 ))
                 (newline file)
                 ))
             )))))))
(define simpleCompiler (make-compiler "Assembly.txt" "Compiled.txt"))
(simpleCompiler 'firstPass)
(simpleCompiler 'secondPass)