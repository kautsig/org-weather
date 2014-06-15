;;; org-weather.el --- Supports displaying the weather forecast from
;;; openweathermap.org in your org-mode agenda.

;; This is an example snippet as retrieved from openweathermap.org
;;
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

;; Define the format string to display in the agenda, see below for available wildcards
(defvar org-weather-format "Weather: %desc, %tmin-%tmax%tu, %p%pu, %h%hu, %s%su")

;; The api url, no need to change ususally
(defvar org-weather-api-url "http://api.openweathermap.org/data/2.5/forecast/daily?q=%s&mode=json&units=metric&cnt=7")
(defvar org-weather-api-timeout 2)

;; The units, just for displaying
(defvar org-weather-temperature-unit "°C")
(defvar org-weather-pressure-unit "hpa")
(defvar org-weather-humidity-unit "%")
(defvar org-weather-speed-unit "m/s")

;; Variables for internal use only
(defvar org-weather-initialized nil)
(defvar org-weather-data)
(setq org-weather-data (make-hash-table :test 'equal))

(defun org-weather-load ()
  "Loads the webserivce data and returns it as a JSON object."
  (with-timeout (org-weather-api-timeout)
      (with-temp-buffer
        (url-insert-file-contents
         (format org-weather-api-url org-weather-location))
        (json-read))))

(defun org-weather-add-to-cache (item)
  "Adds the one result 'list' item to the data cache"
  (let* ((timestr (cdr (assoc 'dt item)))
         (cache-key (format-time-string "%F" (seconds-to-time timestr)))

         ;; extract fields from the 'weather' element
         (weather (cdr (assoc 'weather item)))
         (main (org-weather-weather weather 'main))
         (desc (org-weather-weather weather 'description))
         (icon (org-weather-weather weather 'icon))

         ;; extract fields from the 'temp' element
         (temperature (cdr (assoc 'temp item)))
         (tmin (number-to-string (round (org-weather-temp temperature 'min))))
         (tmax (number-to-string (round (org-weather-temp temperature 'max))))
         (tmorn (number-to-string (round (org-weather-temp temperature 'morn))))
         (tday (number-to-string (round (org-weather-temp temperature 'day))))
         (teve (number-to-string (round (org-weather-temp temperature 'eve))))
         (tnight (number-to-string (round (org-weather-temp temperature 'night))))

         ;; other fields
         (pressure (number-to-string (round (cdr (assoc 'pressure item)))))
         (humidity (number-to-string (round (cdr (assoc 'humidity item)))))
         (speed (number-to-string (round (cdr (assoc 'speed item)))))

         ;; replace in org-weather-format
         (agenda-str org-weather-format)

         (agenda-str (org-weather-replace "%main" main agenda-str))
         (agenda-str (org-weather-replace "%desc" desc agenda-str))
         (agenda-str (org-weather-replace "%icon" icon agenda-str))

         (agenda-str (org-weather-replace "%tu" org-weather-temperature-unit agenda-str))
         (agenda-str (org-weather-replace "%tmin" tmin agenda-str))
         (agenda-str (org-weather-replace "%tmax" tmax agenda-str))
         (agenda-str (org-weather-replace "%tmorn" tmorn agenda-str))
         (agenda-str (org-weather-replace "%tday" tday agenda-str))
         (agenda-str (org-weather-replace "%teve" teve agenda-str))
         (agenda-str (org-weather-replace "%tnight" tnight agenda-str))

         (agenda-str (org-weather-replace "%pu" org-weather-pressure-unit agenda-str))
         (agenda-str (org-weather-replace "%p" pressure agenda-str))
         (agenda-str (org-weather-replace "%hu" org-weather-humidity-unit agenda-str))
         (agenda-str (org-weather-replace "%h" humidity agenda-str))
         (agenda-str (org-weather-replace "%su" org-weather-speed-unit agenda-str))
         (agenda-str (org-weather-replace "%s" speed agenda-str))

         )
    (puthash cache-key agenda-str org-weather-data)))

(defun org-weather-replace (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

(defun org-weather-weather (weather field)
  "Extracts a given field from the first element in the 'weather' list"
  (cdr (assoc field (elt weather 0))))

(defun org-weather-temp (temperature field)
  "Extracts a given field element from a 'temp' element"
  (cdr (assoc field temperature)))

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
      (when org-weather-raw org-weather-raw))))

(defun org-weather ()
  "Usable as sexp expression in the diary or an org file."
  (org-weather-string date))

;; (org-weather-refresh)
;; (eval-when-compile (require 'subr-x))
;; (hash-table-keys org-weather-data)
;; (setq org-weather-initialized nil)
;; (puthash "2014-06-01" "foobarbaz" org-weather-data)
;; (gethash "2014-06-01" org-weather-data)

(provide 'org-weather)
