(import (rnrs)
	(rnrs eval)
	(srfi :64))

(test-begin "Binding tests")

(test-assert (eval 'http-request? (environment '(net http request))))
(test-assert (eval 'http-response? (environment '(net http response))))
(test-assert (eval 'http-context? (environment '(net http context))))
(test-assert (eval '<http-context> (environment '(net http))))

(cond-expand
 ((library (paella))
  (test-assert (eval 'paella-http-request? (environment '(net http paella)))))
 (else #t))
(cond-expand
 ((library (sagittarius nginx))
  (test-assert (eval 'nginx-http-request? (environment '(net http nginx)))))
 (else #t))

(test-end)
