(in-package :cl-user)
(defpackage hackrsa-test
  (:use :cl :hackrsa :prove))
(in-package :hackrsa-test)

;; NOTE: To run this test file, execute `(asdf:test-system :hackrsa)' in your Lisp.

(plan nil)
(setf prove:*enable-colors* t)
(diag "Run Test")

;; common-modulus-attack
(diag "Test: common-modulus-attack")

(subtest "mod-expt"
  (let ((b 10)
	(e 11)
	(m 12))    
    (is (hackrsa::mod-expt b e m)
	(mod (expt b e) m)))
  (is (hackrsa::mod-expt 10 -2 10)
      1/100)
  (is (hackrsa::mod-expt -1 10 20)
      1)
  (is (hackrsa::mod-expt -1 9 20)
      19))

(subtest "extend-gcd"
  (is (hackrsa::extend-gcd 101 102)
      (cons 101 -100))
  (let* ((a 103)
	 (b 102)
	 (ret (hackrsa::extend-gcd a b)))
    (is (+ (* a (car ret)) (* b (cdr ret)))
	1)))


(subtest "common-modulus-attack"
  (let ((e1 11)
	(n1 236934049743116267137999082243372631809789567482083918717832642810097363305512293474568071369055296264199854438630820352634325357252399203160052660683745421710174826323192475870497319105418435646820494864987787286941817224659073497212768480618387152477878449603008187097148599534206055318807657902493850180695091646575878916531742076951110529004783428260456713315007812112632429296257313525506207087475539303737022587194108436132757979273391594299137176227924904126161234005321583720836733205639052615538054399452669637400105028428545751844036229657412844469034970807562336527158965779903175305550570647732255961850364080642984562893392375273054434538280546913977098212083374336482279710348958536764229803743404325258229707314844255917497531735251105389366176228741806064378293682890877558325834873371615135474627913981994123692172918524625407966731238257519603614744577)
	(c1 80265690974140286785447882525076768851800986505783169077080797677035805215248640465159446426193422263912423067392651719120282968933314718780685629466284745121303594495759721471318134122366715904)
	(e2 13)
	(n2 236934049743116267137999082243372631809789567482083918717832642810097363305512293474568071369055296264199854438630820352634325357252399203160052660683745421710174826323192475870497319105418435646820494864987787286941817224659073497212768480618387152477878449603008187097148599534206055318807657902493850180695091646575878916531742076951110529004783428260456713315007812112632429296257313525506207087475539303737022587194108436132757979273391594299137176227924904126161234005321583720836733205639052615538054399452669637400105028428545751844036229657412844469034970807562336527158965779903175305550570647732255961850364080642984562893392375273054434538280546913977098212083374336482279710348958536764229803743404325258229707314844255917497531735251105389366176228741806064378293682890877558325834873371615135474627913981994123692172918524625407966731238257519603614744577)
	(c2 14451037575679461333658489727928902053807202350950440400755535465672646289383249206721118279217195146247129636809289035793102103248547070620691905918862697416910065303500798664102685376006097589955370023822867897020714767877873664))
    (is (print (common-modulus-attack c1 c2 e1 e2 n1))
	424311244315114354)))

;; low-public-exponent-attack
(diag "Test: low-public-exponent-attack")
(subtest "low-public-exponent-attack"
  (let ((e 13)
	(m 131)
	(n 236934049743116267137999082243372631809789567482083918717832642810097363305512293474568071369055296264199854438630820352634325357252399203160052660683745421710174826323192475870497319105418435646820494864987787286941817224659073497212768480618387152477878449603008187097148599534206055318807657902493850180695091646575878916531742076951110529004783428260456713315007812112632429296257313525506207087475539303737022587194108436132757979273391594299137176227924904126161234005321583720836733205639052615538054399452669637400105028428545751844036229657412844469034970807562336527158965779903175305550570647732255961850364080642984562893392375273054434538280546913977098212083374336482279710348958536764229803743404325258229707314844255917497531735251105389366176228741806064378293682890877558325834873371615135474627913981994123692172918524625407966731238257519603614744577))
    (is (low-public-exponent-attack (mod (expt m e) n) e)
	m)))


;; fermat-rules-attack
(diag "Test: fermat-rules-attack")
(skip 1 "Too much time")
;; (subtest "fermat-rules-attack"
;;   (let* ((mid (* 2996863034895 (expt 2 1290000)))
;; 	 (p (1+ mid))
;; 	 (q (1- mid)))
;;     (is (fermat-rules-attack (* p q))
;; 	(cons p q))))

;; wiener-attack
(diag "Test: winer-attack")
(subtest "continued-fraction"
  (is (hackrsa::continued-fraction 4/11)
      '(0 2 1 3))
  (is (hackrsa::continued-fraction 4)
      '(4)))

(subtest "const-continued-fraction"
  (is (hackrsa::const-continued-fraction '(4))
      4)
  (is (hackrsa::const-continued-fraction (hackrsa::continued-fraction 4/11))
      4/11)
  (is (hackrsa::const-continued-fraction nil)
      nil)
  (let ((d 10001)
	(n 10002))
    (is (hackrsa::const-continued-fraction (hackrsa::continued-fraction (/ d n)))
	(/ d n))))

(subtest "guess-f"
  (let ((list (list 0 2 1 3)))    
    (is (hackrsa::guess-f list)
	'(1 1/2 2/5 4/11))))

(subtest "perfect-square-p"
  (ok (hackrsa::perfect-square-p 4))
  (ok (not (hackrsa::perfect-square-p 5)))
  (ok (hackrsa::perfect-square-p (expt 10001 2)))
  (ok (not (hackrsa::perfect-square-p (1+ (expt 10001 2))))))

(subtest "fast-perfect-square-p"
  (let ((i (expt 10001 2)))    
    (is (hackrsa::perfect-square-p i)
	(hackrsa::fast-perfect-square-p i))))

;; TODO implement rsa parameter
;; (subtest "secret-key-p"
;;   (ok ))

(subtest "wiener-attack"
  (let ((e #x466a169e8c14ac89f39b5b0357effc3e2139f9b19e28c1e299f18b54952a07a932ba5ca9f4b93b3eaa5a12c4856981ee1a31a5b47a0068ff081fa3c8c2c546feaa3619fd6ec7dd71c9a2e75f1301ec935f7a5b744a73df34d21c47592e149074a3ccef749ece475e3b6b0c8eecac7c55290ff148e9a29db8480cfe2a57801275)
	(n #x9C2F6505899120906E5AFBD755C92FEC429FBA194466F06AAE484FA33CABA720205E94CE9BF5AA527224916D1852AE07915FBC6A3A52045857E0A1224C72A360C01C0CEF388F1693A746D5AFBF318C0ABF027661ACAB54E0290DFA21C3616A498210E2578121D7C23877429331D428D756B957EB41ECAB1EAAD87018C6EA3445)
	(d #x30273E11CBE5AE0CF9054376C76452F5EF9642C4A0D485FBE6AE6E808FF0E011))    
    (is (wiener-attack n e)
	d)))

(finalize)
