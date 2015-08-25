#lang racket

;; Provide settings files procedures.
(provide supportDirectory
         bridgeSettingsFile
         supportDirectoryExists?
         bridgeSettingsFileExists?)

;; Provide patch procedures.
(provide set-patch!
         set-patch-to-default!)

;; Bridge Settings are currently stored in a hash in the "Bridge Settings.shc"
;; file within the Application Support directory. Ideally, this will eventually
;; become a plist file within ~/Libarary/Preferences, but I am unable to get
;; xml/plist to work at the moment.

;; Long-term TUDU: Test supportDirectory on Linux and Windows.

;; Define the location of the bridge settings file and the file itself.

(define supportDirectory
  (let ([system (system-type 'os)])
    (cond
      ((equal? system 'macosx)
       (string->path (string-append 
                      (path->string (find-system-path 'home-dir)) 
                      "Library/Application Support/Simple Hue Control/")))
      ((equal? system 'unix)
       (string->path (string-append
                      (path->string (find-system-path 'doc-dir))
                      "Simple Hue Control/Settings/")))
      ((equal? system 'windows)
       (string->path (string-append
                      (path->string (find-system-path 'doc-dir))
                      "Simple Hue Control\\Settings\\"))))))

(define bridgeSettingsFile
  (build-path supportDirectory (string->path "Bridge Settings.shc")))

;; Procedures for determining if the support directory and settings file exist
;; and creating them if they do not.

(define supportDirectoryExists?
  (lambda ()
    (cond
      ((not (directory-exists? supportDirectory))
       (make-directory supportDirectory)
       #f)
       (else #t))))

(define bridgeSettingsFileExists?
  (lambda ()
    (cond
      ((not (file-exists? bridgeSettingsFile))
       (write-to-file
        (hash 'bridge-address "0.0.0.0"
              'user-device ""
              'hue-user-name ""
              'app-name "simple_hue_control"
              'device-type "")
        bridgeSettingsFile)
       #f)
      (else #t))))
