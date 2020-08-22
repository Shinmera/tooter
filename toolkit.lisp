#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.tooter)

(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))
(defun universal->unix (universal)
  (- universal *unix-epoch-difference*))

(defun unix->universal (unix)
  (+ unix *unix-epoch-difference*))

(defun universal->utc-timestring (universal)
  (multiple-value-bind (seconds minutes hours day month year)
      (decode-universal-time universal 0)
    (format NIL
            "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hours minutes seconds)))

(defun parse-timestring (string)
  (let* ((y -1)
         (x (position #\- string))
         (d (position #\- string :start (1+ x)))
         (h (position #\T string :start (1+ d)))
         (m (position #\: string :start (1+ h)))
         (s (position #\: string :start (1+ m)))
         (ms (position #\. string :start (1+ s))))
    (flet ((part (s e)
             (parse-integer string :start s :end e)))
      (encode-universal-time
       (part (1+ s) ms)
       (part (1+ m) s)
       (part (1+ h) m)
       (part (1+ d) h)
       (part (1+ x) d)
       (part (1+ y) x)
       0))))

(defun convert-timestamp (stamp)
  (etypecase stamp
    (integer (unix->universal stamp))
    (string (parse-timestring stamp))))

(defun to-keyword (a)
  (intern (with-output-to-string (out)
            (loop for char across a
                  do (if (char= #\_ char)
                         (write-char #\- out)
                         (write-char (char-upcase char) out))))
          "KEYWORD"))

(defun url-encode (thing &optional (external-format :utf-8))
  (with-output-to-string (out)
    (loop for octet across (babel:string-to-octets thing :encoding external-format)
          for char = (code-char octet)
          do (cond ((or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (find char "-._~" :test #'char=))
                    (write-char char out))
                   (T (format out "%~2,'0x" (char-code char)))))))

(defun translate-key (key)
  (with-output-to-string (out)
    (loop for char across (string key)
          do (if (char= char #\-)
                 (write-char #\_ out)
                 (write-char (char-downcase char) out)))))

(defun param-plist->alist (plist)
  (flet ((param-value (val)
           (etypecase val
             (pathname val)
             ((eql T) "true")
             ((eql NIL) "false")
             (integer (princ-to-string val))
             (string val))))
    (loop for (key val) on plist by #'cddr
          nconc (typecase val
                  (list (loop with field = (format NIL "~a[]" (translate-key key))
                              for v in val collect (cons field (param-value v))))
                  (T (list (cons (translate-key key) (param-value val))))))))

(defun make-url (base &rest parameters)
  (format NIL "~a?~{~a=~a~^&~}" base
          (loop for (key val) on parameters by #'cddr
                when val collect (translate-key key)
                when val collect (url-encode val))))

(defun %getj (data &rest attributes)
  (if (null attributes)
      data
      (let ((attribute (first attributes)))
        (apply #'getj
               (etypecase attribute
                 (string (gethash attribute data))
                 (integer (elt data attribute)))
               (rest attributes)))))

(defun getj (data &rest attributes)
  (apply #'%getj data (loop for attribute in attributes
                            collect (etypecase attribute
                                      ((or number string) attribute)
                                      (keyword (translate-key attribute))))))

(define-compiler-macro getj (&environment env data &rest attributes)
  (let ((gensym (gensym "ATTRIBUTE")))
    `(%getj ,data ,@(loop for attribute in attributes
                          for form = `(let ((,gensym ,attribute))
                                        (etypecase ,gensym
                                          ((or number string) ,gensym)
                                          (keyword (translate-key ,gensym))))
                          collect (if (constantp attribute env)
                                      `(load-time-value ,form)
                                      form)))))

(defun line-wrap (string &key (width 80) (prefix ""))
  (with-output-to-string (out)
    (flet ((out (start end)
             (write-string prefix out)
             (write-string string out :start start :end end)
             (when end (terpri out))))
      (loop with last-space = 0
            with line-start = 0
            for i from 0 below (length string)
            for char = (char string i)
            do (cond ((char= #\Space char)
                      (setf last-space i))
                     ((char= #\Linefeed char)
                      (out line-start i)
                      (setf last-space (1+ i)
                            line-start (1+ i)))
                     ((<= width (- i line-start))
                      (cond ((< line-start last-space)
                             (out line-start last-space)
                             (setf i (1+ last-space)))
                            (T
                             (out line-start i)))
                      (setf line-start i)))
            finally (out line-start NIL)))))

(defvar *html-escape-table*
  '(("apos" . "'")
    ("quot" . "\"")
    ("lt" . "<")
    ("gt" . ">")
    ("amp" . "&")))

(defun plain-format-html (string)
  (flet ((r (reg rep str)
           (cl-ppcre:regex-replace-all reg str rep)))
    (cl-ppcre:regex-replace-all
     "&\\w+;"
     (r "<([^>]*)>" ""
        (r "<br\\s*?/?>" (string #\Linefeed)
           string))
     (lambda (string s e ms me rs re)
       (declare (ignore s e rs re))
       (print (list ms me (subseq string ms me)))
       (or (loop for (key . val) in *html-escape-table*
                 do (when (string= key string :start2 (1+ ms) :end2 (1- me))
                      (return val)))
           (subseq string ms me))))))

(defun coerce-boolean (boolean provided)
  (when provided (if boolean "true" "false")))

(defgeneric ensure-integer (object))

(defmethod ensure-integer ((object integer))
  object)

(defmethod ensure-integer ((object string))
  (parse-integer object))
