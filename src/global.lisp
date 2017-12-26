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

;;;; The global acoustic model

(defvar *acoustic-model* nil
  "The currently loaded acoustic model which is comprised of individual phoneme models.")

(defvar *lm* nil
  "The currently loaded language model that is used for decoding.")

(defvar *phone-prefix* "+++PHONE+++"
  "This prefix designates a word as being actually a phone, not a true word.")
