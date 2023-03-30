;;;; -*-coding: utf-8;-*-

(in-package :grieg)

(define-fq "demo-1-no"
  :title "Grunnleggende opplysninger"
  :localization :no
  :genderized nil
  :next "FORTSETT"
  :back "TILBAKE"
  :items
  `((:sc "Oppgi ditt kjønn."
     :opt ("Kvinne"
           "Mann"
           ("Annet" :subq ((:te "Vennligst oppgi:")))))
    (:te "Hvilket år er du født?"
     :verifier ,(in-interval-checker 1900 2022 :message "Verdien må være mellom ~a og ~a."))
    (:sc "Angi området du bor i."
     :opt ("En enkeltgård"
          "En landsby eller en by med under 20.000 innbyggere"
          "En by med mellom 20.001 og 50.000 innbyggere"
          "En by med mellom 50.001 og 100.000 innbyggere"
          "En by med over 100.000 innbyggere"))
    (:sc "Hva er det høyeste utdanningsnivået du har bestått?"
     :opt ("Grunnskole"
           "Videregående skole"
           "Fagopplæring"
           "Universitets- eller høyskolegrad"
           ("Annet" :subq ((:te "Vennligst oppgi:")))))
    (:sc "Har du barn?"
     :opt ("Ja"
           "Nei"))
    (:sc "Er yrket ditt knyttet til klima- eller miljørelatert aktivitet 
      (f.eks. innenfor forskning, undervisning, samfunnsarbeid eller politikk)?"
     :opt ("Ja"
           "Nei"))
    (:sc "Er du engasjert i en organisasjon som arbeider for klimaet eller naturmiljøet?"
     :opt ("Ja"
           "Nei"))
    (:sc "Du har kanskje hørt snakk om at jordens klima er i ferd med 
      å forandre seg på grunn av temperaturøkningen de siste 100 årene. Hva er
      din personlige mening om denne saken? Tror du at klimaet på jorden
      endrer seg?"
     :opt (("Det endrer seg sikkert" :id "o1")
           ("Det endrer seg trolig" :id "o2")
           ("Det endrer seg trolig ikke" :id "o3")
            "Det endrer seg sikkert ikke")
     :id "c1")
    (:scale "Bruk skalaen nedenfor for å angi i hvor stor grad du er bekymret for klimaendringer."
     :helptext "1 på skalaen betyr at du er bekymret i svært liten grad, og 5 betyr at du er bekymret 
     i svært stor grad. Resten av tallene brukes til å uttrykke en mellomting."
     :visible-if (:or (:selected "c1" "o1") (:selected "c1" "o2") (:selected "c1" "o3"))
     :ends (1 5)
     :descriptions ("I svært liten grad" "I svært stor grad"))))
