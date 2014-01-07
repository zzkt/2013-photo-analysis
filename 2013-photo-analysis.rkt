#lang scheme
;; Extract various info from photos on Flickr 
;;     (in particular http://www.flickr.com/photos/zzkt/sets/72157632421623064/ 

;; Copyright: LGPL 2014 nik gaffney
;; Author: nik gaffney <nik@fo.am>
;; Requirements: tested with Racket 5.3.6  

;; things to analyse in 02013 set
;;  - places where photos have been taken (via country code tags, cities, etc)
;;  - subjective trends, most frequently used tags possibly compare with 2012 (tags)
;;  - outliers. unique tags, unique subject, etc (via tags & eyes)
;;  - most intersting/popular (via 'interestingness' and view/favourite counts)
;;  - time of day distribution (possibly heatmap)
;;  - relevant groups, e.g. number of photos, views, distribution in groups
;; 
;; further
;;  - patterns not apparent in visual analysis
;;  - clusters, patterns, correlations in tags, groups, views
;;  - subject analysis (autoclasification)
;;  - photos with most unique tags
;;  - what do popular/interesting photos have in common?
;;  - intersection of personal favourites and 'uninteresting' 
;;
;; analysis of 02013 
;;    (analyse-365)
;;
;; montage
;;  - see imagemagick notes

(require (planet dvanhorn/flickr:2:3))
(include "flickr-auth.rkt") ;; defines current-sec-key and current-api-key

(flickr.test.echo)

(define zzkt "52731283@N06")
(define set_02013 "72157632421623064")

;; get list of photos, with views and tags
(define (get-photoset set)
  (flickr.photosets.getPhotos #:photoset_id set #:extras "views,tags,date_taken"))

;; feature extraction
(define (extract feature photos)
  "rather fragile method to extract details from a photset. 
   feature can include 'id 'tags 'title 'views 'datetaken etc."
  (map (lambda (x)
         (cadr (assoc feature (cadr x))))
       (list-tail (car photos) 2)))

;; sorted and unsorted list of photo titles and views
(define (titles-and-views photos)
  "sort list of photos by views"
  (sort  (map cons (extract 'title photos)
              (map string->number (extract 'views photos)))
         >  #:key cdr))

(define (titles-and-views-unsorted photos)
  (map cons (extract 'title photos)
       (map string->number (extract 'views photos))))

;; extract tags to hash with tag counts
(define (tag-freq photos)
  (let ((ph (make-hash))
        (taglist (string-split (string-join (extract 'tags photos)))))
    (map (lambda (x)
           (hash-update! ph x add1 0))
         taglist)
    (sort (hash-map ph cons) > #:key cdr)))

(define (total-tags photos)
  (length (tag-freq photos)))

(define (unique-tags photos)
  (map car 
       (filter (lambda (x) (= 1 (cdr x))) 
               (tag-freq photos))))

;; time and date
(define (time-diffusion photos)
  (map car 
       (map (lambda (x) (cdr (string-split x)))
            (extract 'datetaken photos))))

;; sort images by 'interestingness'
(define (most-interesting)
  (flickr.photos.search #:user_id zzkt
                        #:sort "interestingness-desc" 
                        #:min_taken_date "2013-01-01 00:00:00" 
                        #:per_page "500" #:extras "views"))

;; update titles and descriptions
;;    -> https://secure.flickr.com/services/api/flickr.photos.setMeta.html

(define (set-title photo title)
  (flickr.photos.setMeta #:photo_id photo #:title title #:description " "))

;; favourites 
;;  NOTE: expects #:photo_id whereas photo-views et.al. expect elements of a photoset 
(define (favorite-count ph)
  (string->number 
   (list-ref (assoc 'total 
                    (cadar (flickr.photos.getFavorites #:photo_id ph))) 1)))

;; (favorite-count "10983871953")

;; groups and sets
;;   flickr.photos.getAllContexts   

(define (get-sets ph)
  (let ((context (flickr.photos.getAllContexts  #:photo_id ph)))
    (map (lambda (x) (cadr (assoc 'title (cadr x)))) 
         (filter (lambda (x) (eqv? 'set (car x))) context))))

(define (get-groups ph)
  (let ((context (flickr.photos.getAllContexts  #:photo_id ph)))
    (map (lambda (x) (cadr (assoc 'title (cadr x)))) 
         (filter (lambda (x) (eqv? 'pool (car x))) context))))

(define (get-contexts ph)
  (let* ((context (flickr.photos.getAllContexts  #:photo_id ph))
         (sets (filter (lambda (x) (eqv? 'set (car x))) context))
         (groups (filter (lambda (x) (eqv? 'pool (car x))) context)))
    (list (map (lambda (x) (cadr (assoc 'title (cadr x)))) sets)
          (map (lambda (x) (cadr (assoc 'title (cadr x)))) groups))))

;; (get-contexts  "10983871953")

;; info from individual photos

(define (photo-title ph)
  (list-ref (assoc 'title (cdar ph)) 2))

(define (photo-url ph)
  (list-ref (caddr (assoc 'urls (cdar ph))) 2))

(define (photo-views ph)
  (string->number
   (list-ref (assoc 'views (cadar ph)) 1)))

;; titles, contexts, views and favourites from a photoset...

(define (exfoliate-photoset photos)
  (map (lambda (id)
         (let ((ph (flickr.photos.getInfo #:photo_id id)))
           ;(displayln id)
           (list (photo-title ph)
                 (cons 'views (photo-views  ph))
                 (cons 'favorites (favorite-count id))
                 (cons 'sets (list (get-sets id)))
                 (cons 'groups (list (get-groups id))))))
       (extract 'id (get-photoset photos))))

;; (exfoliate-photoset "72157638504674335")

;; summarise sets as output from exfoliate-photoset
(define (summarise-sets photos)
  (flatten (map (lambda (x) 
                  (cadr (assoc 'groups (cdr x)))) photos)))

;; (summarise-sets exfoliated)

(define (count-groups groups)
  (let ((ph (make-hash)))
    (map (lambda (x)
           (hash-update! ph x add1 0))
         groups)
    (sort (hash-map ph cons) > #:key cdr)))

;; (count-groups (summarise-sets exfoliated))

;; collate various and sundry analysis
(define (analyse-365)
  (let ((photoset (get-photoset set_02013)))
    (displayln ";; titles and views")
    (print (titles-and-views photoset))
    (displayln ";; tag fequency")
    (print (tag-freq photoset))
    (displayln ";; groups and sets")
    (print (count-groups (exfoliate-photoset set_02013)))))
