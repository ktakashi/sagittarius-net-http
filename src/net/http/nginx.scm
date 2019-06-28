;;;
;;;  Copyright (c) 2019 Takashi Kato. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#!nounbound
(library (net http nginx)
    (export make-nginx-http-request nginx-http-request?
	    make-nginx-http-response nginx-http-response?
	    make-nginx-http-context nginx-http-context?)
    (import (rnrs)
	    (rfc :5322)
	    (rfc mime)
	    (sagittarius nginx)
	    (sagittarius regex)
	    (net http context)
	    (net http request)
	    (net http response))

(define-record-type nginx-http-request
  (parent <http-request>)
  (fields raw-request
	  parameters)
  (protocol (lambda (p)
	      (lambda (req)
		(define parameters (nginx-http-parameters req))
		((p (make-nginx-http-context (nginx-request-context req))
		    (nginx-request-uri req)
		    (nginx-request-method req)
		    (nginx-request-input-port req)
		    (nginx-request-cookies req)
		    nginx-http-request-parameters-ref
		    nginx-http-request-headers-ref)
		 req
		 parameters)))))
(define (nginx-http-parameters request)
  (define ht (make-hashtable string-hash string=?))
  (define query (nginx-request-query-string request))
  (define content-type
    (cond ((nginx-request-content-type request) => mime-parse-content-type)
	  ;; default application/octet-stream
	  (else '("application" "octet-stream"))))
  (define content-length (nginx-request-content-length request))
  (define (parse-parameter query)
    (for-each (lambda (p)
		(let* ((k&v (string-split p "="))
		       (k (car k&v))
		       ;; TODO should we pass empty string?
		       (v (if (null? (cdr k&v)) "" (cadr k&v))))
		  (hashtable-update! ht k (lambda (v*) (cons v v*)) '())))
	      (string-split query "&")))
  (when query (parse-parameter query))
  ;; handling x-www-form-urlencoded here
  (when (and (string=? (car content-type) "application")
	     (string=? (cadr content-type) "x-www-form-urlencoded")
	     content-length)
    (cond ((string->number content-length) =>
	   (lambda (n)
	     ;; okey parse it
	     (let ((c (get-bytevector-n (nginx-request-input-port request) n)))
	       (parse-parameter (utf8->string c)))))))
  ht)

(define (nginx-http-request-headers-ref http-request name)
  (let ((nginx-request (nginx-http-request-raw-request http-request)))
    (rfc5322-header-ref* (nginx-request-headers nginx-request) name)))

(define (nginx-http-request-parameters-ref http-request name)
  (let ((parameters (nginx-http-request-parameters http-request)))
    (hashtable-ref parameters name '())))

(define-record-type nginx-http-response
  (parent <http-response>)
  (fields raw-response)
  (protocol (lambda (p)
	      (lambda (response)
		((p nginx-http-response-header-set!
		    nginx-http-response-header-add!
		    (nginx-response-output-port response))
		 response)))))

(define (nginx-http-response-header-set! response name value)
  (nginx-response-header-set! (nginx-http-response-raw-response response)
			      name value))
(define (nginx-http-response-header-add! response name value)
  (nginx-response-header-add! (nginx-http-response-raw-response response)
			      name value))

(define-record-type nginx-http-context
  (parent <http-context>)
  (fields raw-context)
  (protocol (lambda (p)
	      (lambda (context)
		((p (nginx-context-path context)
		    (nginx-context-parameters context))
		 context)))))
)
	    
