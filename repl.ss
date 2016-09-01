;; * MIOGUI *
;;
;; Copyright 2016 Aldo Nicolas Bruno
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(import (nanomsg))
(nanomsg-library-init)
(define my-local-repl-sock (nn-socket AF_SP NN_REP))
(define my-local-repl-eid (nn-bind my-local-repl-sock "tcp://127.0.0.1:9888"))

(define (my-local-repl)
  (call/cc 
   (lambda (return)
     (let* ([buf (box #t)]
	    [r (nn-recv my-local-repl-sock buf NN_MSG NN_DONTWAIT)])
       (when r
	     (printf "in my-local-repl ~d~n" (utf8->string (unbox buf)))
	     (with-input-from-string (utf8->string (unbox buf))
	       (lambda ()
		 (let ([pr (call-with-string-output-port
			    (lambda (p)
			      (parameterize 
			       ([current-output-port p])
				 (guard (e [else (print-condition e) ])
					(let* ([token (read)]
					       [x (eval token (interaction-environment))])
					  (pretty-print x))))))])
			  (nn-send my-local-repl-sock (string->utf8 pr) 0)))))))))

