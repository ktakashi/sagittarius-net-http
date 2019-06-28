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
(library (net http request)
    (export (rename (http-request <http-request>))
	    http-request?
	    http-request-context
	    http-request-uri
	    http-request-method
	    http-request-input-port
	    http-request-cookies
	    http-request-attribute-ref
	    http-request-attribute-set!
	    http-request-attribute-names
	    http-request-header-ref
	    http-request-header-ref*
	    http-request-parameter-ref*
	    http-request-parameter-ref)
    (import (rnrs)
	    (net http context)
	    (util hashtables))

;;; interface
(define-record-type http-request
  (fields context
	  uri
	  method
	  input-port
	  cookies ;; a list of cookie (rfc cookie)
	  attributes
	  parameters-referer
	  headers-referer)
  (protocol (lambda (p)
	      (lambda (ctx uri mth ip cookies params-ref header-ref)
		(unless (http-context? ctx)
		  (assertion-violation 'make-http-request
		   "http-context required for request context" ctx))
		(p ctx uri mth ip cookies
		   (make-hashtable string-hash string=?)
		   params-ref header-ref)))))
(define (http-request-header-ref* request name)
  ((http-request-headers-referer request) request name))
(define (http-request-header-ref request name . maybe-default)
  (define default (and (not (null? maybe-default)) (car maybe-default)))
  (let ((r (http-request-header-ref* request name)))
    (if (null? r)
	default
	(car r))))
(define (http-request-parameter-ref* request name)
  ((http-request-parameters-referer request) request name))
(define (http-request-parameter-ref request name . maybe-default)
  (define default (and (not (null? maybe-default)) (car maybe-default)))
  (let ((r (http-request-parameter-ref* request name)))
    (if (null? r)
	default
	(car r))))

(define (http-request-attribute-ref request name . default)
  (unless (string? name)
    (assertion-violation 'http-request-attribute-ref
			 "Attribute name must be a string" name))
  (apply hashtable-ref (http-request-attributes request) name default))
(define (http-request-attribute-set! request name value)
  (unless (string? name)
    (assertion-violation 'http-request-attribute-set!
			 "Attribute name must be a string" name))
  (hashtable-set! (http-request-attributes request) name value))

(define (http-request-attribute-names request)
  (hashtable-keys-list (http-request-attributes request)))


)
