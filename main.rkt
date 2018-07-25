#lang racket
(require net/http-client
         net/uri-codec
         net/base64
         json)
(require web-server/stuffers/hmac-sha1)
(provide qiniu-upload current-qiniu-access-key
         current-qiniu-secret-key
         current-qiniu-expired-time)

(define current-qiniu-access-key (make-parameter #f))
(define current-qiniu-secret-key (make-parameter #f))
(define current-qiniu-expired-time (make-parameter #f))

(define (base64-encode/url-safe x)
  (string->bytes/utf-8
   (string-replace (string-replace (bytes->string/utf-8 (base64-encode x "")) "+" "-") "/" "_")))

(define boundary (format "-----~a-----files----boundary----~a-----uploader--qiniuyun-" (random 10000)
                         (random 10000)))

(define (qiniu-upload buket port #:key [key #f])
  (define key-segment
    (if key
        (bytes-append (string->bytes/utf-8 (format "\r\n--~a\r\nContent-Disposition: form-data; name=\"key\";\r\n\r\n" boundary))
                     (string->bytes/utf-8 key))
        #""))
  (define send-data (bytes-append
                     (string->bytes/utf-8 (format "--~a\r\nContent-Disposition: form-data; name=\"action\";\r\n\r\n" boundary))
                     #"http://upload-z2.qiniup.com"
                     (string->bytes/utf-8 (format "\r\n--~a\r\nContent-Disposition: form-data; name=\"token\";\r\n\r\n" boundary))
                     (make-qiniu-token (if key (format "~a:~a" buket key)
                                           buket))
                     key-segment
                     (string->bytes/utf-8 (format "\r\n--~a\r\nContent-Disposition: form-data; name=\"file\"; filename=\"test.jpg\"\r\nContent-Type: application/octet-stream\r\n\r\n" boundary))
                     (port->bytes port)
                     #"\r\n--"
                     (string->bytes/utf-8 boundary)
                     #"--"))
  (close-input-port port)
  (define-values (a b c)
    (http-sendrecv "upload.qiniu.com"
                   "http://upload.qiniup.com/"
                   #:headers
                   (list (format
                          "Content-Type: multipart/form-data; boundary=~a" boundary)
                         (format
                          "Content-Length : ~a" (bytes-length send-data))
                         )
                   #:data
                   send-data
                   #:method #"POST"))
  (if (equal? #"HTTP/1.1 200 OK" a)
      (hash-ref (read-json c) 'key)
      #f)
  
  )

(define (make-qiniu-token buket)
  (define access-key (current-qiniu-access-key))
  (define secret-key (current-qiniu-secret-key))
  (define expired-time (current-qiniu-expired-time))
  (unless (bytes? access-key)
    (error "access-key hasn't been set correctly."))
  (unless (bytes? secret-key)
    (error "secret-key hasn't been set correctly."))
  (unless (integer? expired-time)
    (error "expired time hasn't been set correctly."))
  (define json
    (string->bytes/utf-8 (with-output-to-string
      (thunk (write-json
              (hasheq 'scope buket
                      'deadline (+ (current-seconds) (* 3600 24 expired-time))
                      ))))))
  (define json-encoded (base64-encode/url-safe json))
  (define json-sha1 (HMAC-SHA1 secret-key json-encoded))
  (define encoded-sign (base64-encode/url-safe json-sha1))
  (bytes-append access-key #":" encoded-sign #":" json-encoded))

  