
;(make-rmlp accel641-R 6 1 0 4)
(make-rmlp accel641-R2 6 1 0 4)
(make-rmlp accel641-R3 6 1 0 4)
(make-rmlp accel641-R4 6 1 0 4)


(setf (net accel641-R2) (copy-tree (net accel641-R)))
(setf (net accel641-R3) (copy-tree (net accel641-R)))
(setf (net accel641-R4) (copy-tree (net accel641-R)))


(setf (udplist accel641-r) 11001
      (iplist accel641-r) "127.0.0.1"
      (latence accel641-r) 0
      (superdaemon accel641-r) T
      (udplist accel641-r2) 11002
      (iplist accel641-r2) "127.0.0.1"
      (latence accel641-r2) 0
      (superdaemon accel641-r2) T
      (udplist accel641-r3) 11003
      (iplist accel641-r3) "127.0.0.1"
      (latence accel641-r3) 0
      (superdaemon accel641-r3) T
      (udplist accel641-r4) 11004
      (iplist accel641-r4) "127.0.0.1"
      (latence accel641-r4) 0
      (superdaemon accel641-r4) T
      (output accel641-r2) '(0)
      (output accel641-r3) '(0)
      (output accel641-r4) '(0))


;; then, create threads to listen data comming from Puredata and to send it back the MLP output
(setf (daemons accel641-r)
      (list (sb-thread:make-thread (lambda () (input-server accel641-r 11001 128))
				   :name "listen1")
	    (sb-thread:make-thread (lambda () (output-server accel641-r "127.0.0.1" 10001))
				   :name "answer1")))

(setf (daemons accel641-r2)
      (list (sb-thread:make-thread (lambda () (input-server accel641-r2 (udplist accel641-r2) 128))
				   :name "listen2")
	    (sb-thread:make-thread (lambda () (output-server accel641-r2 (iplist accel641-r2) 10002))
				   :name "answer2"))
      (daemons accel641-r3)
      (list (sb-thread:make-thread (lambda () (input-server accel641-r3 (udplist accel641-r3) 128))
				   :name "listen3")
	    (sb-thread:make-thread (lambda () (output-server accel641-r3 (iplist accel641-r3) 10003))
				   :name "answer3"))
      (daemons accel641-r4)
      (list (sb-thread:make-thread (lambda () (input-server accel641-r4 (udplist accel641-r4) 128))
				   :name "listen4")
	    (sb-thread:make-thread (lambda () (output-server accel641-r4 (iplist accel641-r4) 10004))
				   :name "answer4")))

      

(push (sb-thread:make-thread (lambda () (output-server accel641-r2 (iplist accel641-r2) 10002))
			     :name "answer2")
      (daemons accel641-r2))

(push (sb-thread:make-thread (lambda () (output-server accel641-r3 (iplist accel641-r3) 10003))
			     :name "answer3")
      (daemons accel641-r3))

(push (sb-thread:make-thread (lambda () (output-server accel641-r4 (iplist accel641-r4) 10004))
			     :name "answer4")
      (daemons accel641-r4))


;(sb-thread:terminate-thread (car (daemons accel641-r3)))

(mapcar #'(lamnbda (x) (setf (superdaemon x) nil))
	(list accel641-r accel641-r2 accel641-r3 accel641-r4))

;(mapcar #'sb-thread:terminate-thread (daemons accel641-r))
;(mapcar #'sb-thread:terminate-thread (daemons accel641-r2))
;(mapcar #'sb-thread:terminate-thread (daemons accel641-r3))
;(mapcar #'sb-thread:terminate-thread (daemons accel641-r4))
