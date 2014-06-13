;; {
;;   "list": [
;;     {
;;       "dt": 1401616800,
;;       "temp": {
;;         "day": 17.04,
;;         "min": 11.19,
;;         "max": 17.04,
;;         "night": 11.19,
;;         "eve": 16.19,
;;         "morn": 17.04
;;       },
;;       "pressure": 934.7,
;;       "humidity": 56,
;;       "weather": [
;;         {
;;           "id": 803,
;;           "main": "Clouds",
;;           "description": "broken clouds",
;;           "icon": "04d"
;;         }
;;       ],
;;       "speed": 1.52,
;;       "deg": 356,
;;       "clouds": 68
;;     }
;;   ]
;; }

(require 'json)
(require 'url)

;; Set this to your location
(defvar org-weather-location "Graz,AT")

;; Variables for internal use only
(defvar org-weather-initialized nil)
(defvar org-weather-data)
(setq org-weather-data (make-hash-table :test 'equal))

(defun org-weather-load ()
  "Loads the webserivce data and returns it as a JSON object."
  (with-temp-buffer
    (url-insert-file-contents
     (format "http://api.openweathermap.org/data/2.5/forecast/daily?q=%s&mode=json&units=metric&cnt=7" org-weather-location))
    (json-read)))

(defun org-weather-add-to-cache (item)
  "Adds the one result 'list' item to the data cache"
  (let* ((timestr (cdr (assoc 'dt item)))
         (cache-key (format-time-string "%F" (seconds-to-time timestr)))
         (weather (cdr (assoc 'weather item))))
    (puthash cache-key (org-weather-format weather) org-weather-data)))

(defun org-weather-format (item)
  "Extracts the description from a 'weather' element"
  (cdr (assoc 'description (elt item 0))))

(defun org-weather-refresh ()
  "Refreshes the weather data"
  (interactive)
  (setq org-weather-data (make-hash-table :test 'equal))
  (mapcar 'org-weather-add-to-cache (cdr (assoc 'list (org-weather-load))))
  (setq org-weather-initialized t))

(defun org-weather-string (&optional date)
  "String of day number of year of Gregorian DATE. Defaults to today's date if DATE is not given."
  (let* ((d (or date (calendar-current-date)))
         (year (calendar-extract-year d))
         (month (calendar-extract-month d))
         (day (calendar-extract-day d))
         (current-time (encode-time 0 0 0 day month year))
         (cache-key (format-time-string "%F" current-time)))
    (when (not org-weather-initialized)
      (org-weather-refresh)
      (setq org-weather-initialized t))
    (let ((org-weather-raw (gethash cache-key org-weather-data)))
      ;; (format "Weather: %s" org-weather-raw)
      (org-weather-raw)
      ;; ("foo")
      )))

(defun org-weather ()
  "Usable as sexp expression in the diary or an org file."
  (org-weather-string date))

;; (org-weather-refresh)
;; (eval-when-compile (require 'subr-x))
;; (hash-table-keys org-weather-data)
;; (setq org-weather-initialized nil)
;; (puthash "2014-06-01" "foobarbaz" org-weather-data)
;; (gethash "2014-06-01" org-weather-data)
