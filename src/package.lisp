;; Copyright 2010-2018 Ben Lambert

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


;; This probably isn't the right place for this... but it's as good as any?
;;#-darwin
;;(port-audio::pa-initialize)

(defpackage :cl-asr
  ;;(:use :common-lisp :blambert-util :alexandria) ;; :bordeaux-fft :cl-ppcre )
  (:use :common-lisp :alexandria) ;; :bordeaux-fft :cl-ppcre )
  (:import-from :metatilities :defclass* :defclass-brief)
  ;; Alexandria has a 'variance' and 'mean' function that we have to shadow...
  (:shadow :variance :mean)
  (:export :dictionary-word-phoneme-map
	   :read-dictionary))
