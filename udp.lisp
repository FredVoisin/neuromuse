;; neuromuse version 2.0 beta 2
;; LISP code to simulate artificial neural networks

;; (C) Frederic Voisin 2000-2008
;; <fredvoisin@neuromuse.org>, <www.neuromuse.org>

;This program is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 2 of the License, or
;(at your option) any later version.

;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.

;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;The GNU Public Licence can be found in the file COPYING
;------------------------------------------------------------------
;; UDP in lisp

(format t "Loading UDP...~%")

#+sbcl (require :sb-bsd-sockets)

;; au cas ou, peut permettre dans certains cas de ne pas quiter lisp
;; quand surcharge cause par udp
(defvar *udp* t)

(defun udp () (if *udp* (setf *udp* nil) (setf *udp* t) ))

#+sbcl (defun send-udp (string host port)
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket
				 :type :datagram :protocol :udp)))
	   (sb-bsd-sockets:socket-connect s (sb-bsd-sockets:make-inet-address host) port)
	   (sb-bsd-sockets:socket-send s (format nil "~a~c~c" string #\return #\linefeed) nil)
	   (sb-bsd-sockets:socket-close s)))

#+sbcl (defun receive-udp (port &optional (lenght 256))
	 (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
	   (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
	   (loop while *udp* do
		 (multiple-value-bind (buf len address port) (sb-bsd-sockets:socket-receive s nil lenght)
		   (print buf)))
	   (sb-bsd-sockets:socket-close s)))

; (setf *udp* t)
; (receive-udp 60000)
; (setf *udp* nil)

#+mcl (defun send-udp (string host port &key verbose)
	(let ((sock (make-socket :format :text :type :datagram)))
	  (send-to sock
		   (format nil "~a~c~c" string #\return #\linefeed)
		   (+ (length string) 2)
		   :remote-host host
		   :remote-port port)
	  (close sock))
	(when verbose (format t "~S ~S closed~%" host port)))

;(send-udp "test" "127.0.0.1" 8888)  ;)

#+mcl (defun listenudp (port size)
	#+mcl
	(let ((sock  (make-socket :format :text
				  :type :datagram
				  :local-port port))
	      (data nil))
	  (setf data (receive-from sock size :extract t))
	  (close sock)
	  (print data)
	  (values (subseq data 0 (- (length data) 2)))))

#+sbcl (defun som-input-server (som port lenght)
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

#+mcl (defun som-input-server (som port length)
	(let ((s (make-socket :format :text
			      :type :datagram
			      :local-port port)))
	  (loop while (superdaemon som) do
		(when (verbose som) (format t "Input server received data on port ~S~%" port))
		(setf (input som) (listenudp s length)
		      (winner som) (find-winner som)
		      (output som) (activation som :n (car (id (winner som)))))
		(sleep (attention som)))
	  (close s)))

#+sbcl (defun som-control-server (som port lenght)
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

#+mcl (defun som-control-server (som port length)
	(let ((s (make-socket :format :text
			      :type :datagram
			      :local-port port)))
	  (loop while (superdaemon som) do
		(when (verbose som) (format t "Input server received data on port ~S~%" port))
		(let* ((cmd-val (receive-from s length))
		       (cmd (car cmd-val))
		       (val (cadr cmd-val)))
		  (eval `(setf (,cmd som) ,val))))
	  (close s)))

(defun som-output-server (som ip port) ;send-udp-activation (som)
  (loop while (superdaemon som) do
	(let ((out (list2string (output som))))
	  (send-udp out ip port)
	  (sleep (latence som)))))

(defun som-activation-server (som ip port) ;send-udp-activation (som)
  (loop while (superdaemon som) do
	(let ((out (list2string (activation som))))
	  (send-udp out ip port)
	  (sleep (latence som)))))

;eof
