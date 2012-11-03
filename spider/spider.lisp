(require 'trivial-http)

(defparameter *url* "http://www.weather-forecast.com/locations/Kyiv/forecasts/latest")
;(trivial-http:http-get "http://google.com")
(defun flatten-headers (headers)
  (let ((result (make-list 2) ))
	(loop for item in headers do
	  (progn
	    (push (cdr item) result)
	    (push (car item) result)))
    result))

;(flatten-headers (second (trivial-http:http-get "http://google.com")))

(defun get-prop (name plist)
  (print (car plist))
  (cond
    ((not (car plist)) (print "nil") nil)
    ((equal name (car plist)) (cadr plist))
    (t (get-prop name (cddr plist)))))

;(get-prop ':CONTENT-LENGTH (flatten-headers (second (trivial-http:http-get "http://google.com"))))

(defun read-responce (responce)
  (let* ((stream (third responce)) (result (make-string 64000)))
    (values result (read-sequence result stream))))
;(read-responce (trivial-http:http-get "http://google.com"))

(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defparameter *response* (trivial-http:http-get *url*))
(print *response*)
(defparameter *headers* (flatten-headers (second *response*)))
(setf *lenth* (read-from-string (get-prop ':CONTENT-LENGTH *headers*)))
(defparameter *data* (read-responce *response*))
((print *data*)
(print *lenth*)
