#|
 This file is a part of Tooter
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.tooter.pleroma)

;;; Notifications

(defmethod delete-notification ((client client) (id string))
  (submit client "/api/v1/notifications/dismiss"
          :id id)
  T)

(defmethod delete-notification ((client client) (notification notification))
  (delete-notification client (id notification)))
