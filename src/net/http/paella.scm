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
(library (net http paella)
    (export make-paella-http-request
	    paella-http-request?
	    make-paella-http-response
	    paella-http-response?
	    paella-http-response-extractor
	    paella-http-response-headers
	    make-paella-http-context
	    paella-http-context?
	    *paella-http-context*)
    (import (rnrs)
	    (prefix (paella) paella:)
	    (net http context)
	    (net http request)
	    (net http response)
	    (binary io)
	    (rfc :5322)
	    (rfc mime)
	    (srfi :39 parameters))

 ;; caller must set
(define *paella-http-context* (make-parameter #f))

(define (content-length headers)
  ;; these 2 are already parsed by the server...
  (define (multipart/form-data? ct)
    (and (string=? (car ct) "multipart")
	 (string=? (cadr ct) "form-data")))
  (define (application/x-www-form-urlencoded? ct)
    (and (string=? (car ct) "application")
	 (string=? (cadr ct) "x-www-form-urlencoded")))
  (let ((ct (cond ((rfc5322-header-ref headers "content-type") =>
		   mime-parse-content-type)
		  (else #f))))
    (cond ((and ct
		(not (multipart/form-data? ct))
		(not (application/x-www-form-urlencoded? ct))
		(rfc5322-header-ref headers "content-length"))
	   => string->number)
	  (else 0))))

(define-record-type paella-http-request
  (parent <http-request>)
  (fields raw-request)
  (protocol (lambda (p)
	      (lambda (request)
		(define headers (paella:http-request-headers request))
		((p (*paella-http-context*)
		    (paella:http-request-uri request)
		    (paella:http-request-method request)
		    (->size-limit-binary-input-port
		     (paella:http-request-source request)
		     (content-length headers))
		    (paella:http-request-cookies request)
		    paella-http-request-parameters-ref
		    paella-http-request-headers-ref)
		 request)))))
(define (paella-http-request-headers-ref request name)
  (let ((paella-request (paella-http-request-raw-request request)))
    (rfc5322-header-ref* (paella:http-request-headers paella-request) name)))
(define (paella-http-request-parameters-ref request name)
  (define paella-request (paella-http-request-raw-request request))
  (cond ((assoc (paella:http-request-parameters paella-request) name) => list)
	(else '())))

(define-record-type paella-http-response
  (parent <http-response>)
  (fields headers
	  extractor)
  (protocol (lambda (p)
	      (lambda ()
		(let ((ht (make-hashtable string-ci-hash string-ci=?)))
		  (let-values (((out extract) (open-bytevector-output-port)))
		    ((p paella-http-response-header-set!
			paella-http-response-header-add!
			out)
		     ht extract)))))))
(define (paella-http-response-header-set! response name value)
  (define headers (paella-http-response-headers response))
  (hashtable-set! headers name (list value)))
  
(define (paella-http-response-header-add! response name value)
  (define headers (paella-http-response-headers response))
  (hashtable-update! headers name (lambda (v*) (cons value v*)) '()))

(define-record-type paella-http-context
  (parent <http-context>))
)
