#|
  This file is a part of neuromuse project.
|#

(defsystem "neuromuse"
  :version "2.12.0"
  :author "Fred Voisin"
  :license " GNU GENERAL PUBLIC LICENSE V3"
  :depends-on (:sb-bsd-sockets)
  :components ((:module "src"
                :components
                ((:file "neuromuse")
		 (:file	"neuromuse-main")
		 (:file "misc")
		 (:file "maths")
		 (:file	"mlp")
		 (:file "perceptron")
		 (:file "som")
		 (:file "rosom")
		 (:file "udp"))))
  :description "Neural nets for experiments in music prod by Fred Voisin, since 1999."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "neuromuse-test"))))

#|
Optional Ltk/Tk visualisation of an MLP (src/gui.lisp), in its own system and
its own package (:neuromuse-gui) so that :ltk stays out of the library's own
:depends-on -- neuromuse must remain loadable and usable headless.
  (ql:quickload :ltk)                    ; once, needs Tk installed
  (asdf:load-system "neuromuse/gui")     ; the package it defines is :neuromuse-gui
Named neuromuse/gui, and not neuromuse-gui, so that ASDF resolves it to this
file (primary system neuromuse) and finds it even in a fresh image.
|#
(defsystem "neuromuse/gui"
  :depends-on (:neuromuse :ltk)
  :components ((:module "src"
                :components
                ((:file "gui"))))
  :description "Ltk visualisation of an MLP's synaptic weights and error curve.")

(defsystem "neuromuse-test"
  :depends-on (:neuromuse :prove)
  :components ((:module "tests"
                :components
                ((:file "neuromuse"))))
  :description "Test suite for neuromuse.")
