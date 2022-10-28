;;;; -*-coding: utf-8;-*-

(in-package :grieg)

(define-fq "demo-1-pl"
  :toc-name "Podstawowe informacje"
  :title "Podstawowe informacje"
  :next "PRZEJDŹ DALEJ"
  :items
  `((:sc "Wskaż swoją płeć."
     :opt ("Kobieta"
          "Mężczyzna"
          ("Inna" :subq ((:te "Jaka?")))))
    (:te "Podaj swój rok urodzenia."  :verifier ,(in-interval-checker 1900 2022))
    (:sc "Wskaż swoje miejsce zamieszkania."
     :opt ("Wieś"
          "Miasto do 50.000 mieszkańców"
          "Miasto od 50.001 do 100.000 mieszkańców"
          "Miasto od 100.001 do 500.000 mieszkańców"
          "Miasto powyżej 500.000 mieszkańców"))
    (:sc "Wskaż swoje wykształcenie."
     :helptext "Chodzi tutaj o ukończoną przez Ciebie szkołę najwyższego szczebla."
     :opt ("Niepełne podstawowe"
           "Podstawowe"
           "Gimnazjalne"
           "Zawodowe lub zasadnicze zawodowe"
           "Średnie"
           "Dyplom licencjacki lub dyplom inżynierski"
           "Dyplom magistra lub dyplom lekarza"
           "Stopień naukowy doktora, doktora habilitowanego lub tytuł profesora"
           ("Inne" :subq ((:te "Jakie?")))))
    (:sc "Czy masz dzieci?"
     :opt ("Tak"
           "Nie"))
    (:sc "Czy Twój zawód jest związany z działalnością (np. naukową, edukacyjną,
    społeczną, polityczną) na rzecz klimatu lub środowiska naturalnego?"
     :opt ("Tak"
           "Nie"))
    (:sc "Czy jesteś zaangażowan[[y//a]] w działalność jakiejś organizacji
    działającej na rzecz klimatu lub środowiska naturalnego?"
     :opt ("Tak"
           "Nie"))
    (:sc "Być może zetkn[[ąłeś//ęłaś]] się z poglądem, że klimat na Ziemi
    zmienia się ze względu na wzrost temperatury w ciągu ostatnich 100 lat.
    Jaka jest Twoja osobista opinia w tej sprawie? Czy uważasz, że klimat na
    Ziemi zmienia się?"
     :opt (("Na pewno się zmienia" :id "o1")
           ("Raczej się zmienia" :id "o2")
           ("Raczej się nie zmienia" :id "o3")
            "Na pewno się nie zmienia")
     :id "c1")
    (:scale "Korzystając z poniższej skali, zaznacz, w jakim stopniu martwisz się zmianą klimatu."
     :helptext "Na poniższej skali 1 oznacza, że martwisz się w bardzo małym stopniu, a 5 – że martwisz się w bardzo dużym stopniu.
     Pozostałe liczby służą do wyrażenia przekonań pośrednich."
     :visible-if (:or (:selected "c1" "o1") (:selected "c1" "o2") (:selected "c1" "o3"))
     :ends (1 5)
     :descriptions ("W bardzo małym stopniu" "W bardzo dużym stopniu"))))
