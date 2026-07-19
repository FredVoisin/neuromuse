;;Frederic Voisin, Nice - Sophia Antipolis, 18 avril 2006
;(format t "Agent serveur SOM pour test cluster Eurecom;~%")
;(format t "le controle s'effectue a l'aide de PureData ou Max.")

(load "neuromuse.lisp")
(load "bordeaux-mp/bordeaux-mp.lisp")
(load "bordeaux-mp/sbcl-bmp.lisp")

(defvar som (make-instance 'som
			   :name 'som
			   :topology '(euclidian 2)
			   :radius 2
			   :neighbourhood 'voisins))

(init-som som 100 8)
(setf (attention som) .05
      (learn-fact som) .1
      (radius som) 2
      (temp som) .01
      (latence som) .1
      (input som) #(0 0 0 0 0 0 0 0)
      (output som) '(0 0 0 0 0 0 0 0))

(defun udp-input-server (som port lenght)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
    (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
    (loop while (superdaemon som) do
     (multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght)
       (when (verbose som)
	 (format t "Received ~A bytes from ~A:~A - ~A ~%"
		 len address port (subseq buf 0 (min 10 len))))
       (setf (input som) (st2v buf)
	     (winner-neuron som) (winner som)
	     (output som) (activation som :n (car (id (winner-neuron som)))))
       (sleep (attention som))))
    (sb-bsd-sockets:socket-close s)))

(defun udp-control-server (som port lenght)
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
    (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
    (loop while (superdaemon som) do
     (multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght)
       (when (verbose som)
	 (format t "Received ~A bytes from ~A:~A - ~A ~%"
		 len address port (subseq buf 0 (min 10 len))))
       (let* ((cmd-val (st2list buf))
	      (cmd (car cmd-val))
	      (val (cadr cmd-val)))
	(eval `(setf (,cmd som) ,val)))))
     (sb-bsd-sockets:socket-close s)))

(defun send-udp-activation (som)
  (loop while (superdaemon som) do
	(let ((out (list2string (output som))))
	  (send-udp out (first (iplist som)) (third (udplist som)))
	  (send-udp out (second (iplist som)) (third (udplist som)))
	  (send-udp out (third (iplist som)) (third (udplist som)))
	  (send-udp out (fourth (iplist som)) (third (udplist som)))
	  (send-udp out (fifth (iplist som)) (third (udplist som)))
	  (sleep (latence som)))))

(setf (udplist som) '(42000 42001 42002 42003)
      (iplist som) '("192.168.0.11" "192.168.0.66" "66.249.93.99" "66.249.93.99" "66.249.93.99" "66.249.93.99" "66.249.93.99")
      (superdaemon som) t)

(setf (daemons som) (list (bordeaux-mp:make-process (lambda () (udp-input-server som (car (udplist som)) 128))
						    :name "listen-input")
			  (bordeaux-mp:make-process (lambda () (udp-control-server som (cadr (udplist som)) 64))
						    :name "listen-control")
			  (bordeaux-mp:make-process (lambda () (send-udp-activation som)) :name "activation")))

;(bordeaux-mp:destroy-process (first (daemons som)))
;(bordeaux-mp:destroy-process (second (daemons som)))
;(bordeaux-mp:destroy-process (third (daemons som)))

(print "Pret !")
;oef
