(ns user
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

(s/def ::non-blank-string (s/and string? (complement str/blank?)))
(s/def ::byr #(->> (Integer. %) (s/int-in-range? 1920 2003)))
(s/def ::iyr #(->> (Integer. %) (s/int-in-range? 2010 2021)))
(s/def ::eyr #(->> (Integer. %) (s/int-in-range? 2020 2031)))
(s/def ::hgt
  (s/or ::cm (s/and string? #(some->> % (re-find #"^(\d{3})cm$") last Integer. (s/int-in-range? 150 194)))
        ::in (s/and string? #(some->> % (re-find #"^(\d{2})in$") last Integer. (s/int-in-range? 59 77)))))
(s/def ::hcl (s/and string? #(re-find #"^#[0-9a-f]{6}$" %)))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid (s/and string? #(re-find #"^[0-9]{9}$" %)))
(s/def ::passport
  (s/keys :req-un [::byr ::eyr ::hcl ::hgt ::iyr ::pid ::ecl]))

(->> (str/split (slurp "input.txt") #"\n\n")
       (map #(-> % (str/replace #"\n" " ") (str/split #"\s")))
       (mapv (fn [x] (map (fn [y] (str/split y #":")) x)))
       (map #(reduce (fn [m [k v]] (assoc m (keyword k) v)) {} %))
       (map #(s/valid? ::passport %))
       (remove false?)
       (count))


;; 233 first answer
(def test-valid "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")


(def test-bad
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")


(def test-input
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")
