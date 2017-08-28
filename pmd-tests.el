(ert-deftest pmd//extract-modifiers-test ()
  (should (equal (list "1,2,3" nil) (pmd//extract-modifiers "1,2,3")))
  (should (equal (list "1,2,3" '("is")) (pmd//extract-modifiers "is/1,2,3")))
  (should (equal (list "1,2,3/a,b" '("is")) (pmd//extract-modifiers "is/1,2,3/a,b")))
  (should (equal (list "1,2,3/a,b" '("it" "works=fine")) (pmd//extract-modifiers "it;works=fine/1,2,3/a,b"))))

(ert-deftest pmd//process-modifiers-test ()
  (should (equal '(let ((pmd-require-escape-input-separator t)) (look ma no hands)) ( pmd--m-require-escape '(look ma no hands))))
  (should (equal '(let ((pmd-require-escape-input-separator nil)) (look ma no hands)) ( pmd--m-require-escape '(look ma no hands) 'false)))
  (should (equal '(let ((pmd-require-escape-input-separator t)) nil) (pmd//process-modifiers '("re") nil)))
  (should (equal '(let ((pmd-require-escape-input-separator nil)) inv) (pmd//process-modifiers '("re!") 'inv)))
  (should (equal '(let ((input (eval (read input)))) (i can lisp !)) (pmd//process-modifiers '("el") '(i can lisp !)))))

(ert-deftest pmd//parse-input-test ()
  (should (equal (list "var1" "var2") (pmd//run-program-in-input-context "re/var1\\,   var2" '(pmd//split-input input))))
  (should (equal (list "[1,2,3]") (pmd//run-program-in-input-context "re/[1,2,3]" '(pmd//split-input input))))
  (should (equal (list "1" "2" "3") (pmd//run-program-in-input-context "1,2,3" '(pmd//split-input input))))
  (should (equal (list "[1" "2" "3]") (pmd//run-program-in-input-context "re!/[1,2,3]" '(pmd//split-input input)))))

(ert-deftest pmd//ruby-prepare-output-test ()
  (pmd//ruby-setup)
  (should (equal "puts \"my-var = #{my-var}\"" (pmd//prepare-output (list "my-var"))))
  (should (equal "puts \"var1 = #{var1} | var2 = #{var2}\"" (pmd//prepare-output (list "var1" "var2")))))

(ert-deftest pmd//js-prepare-output-test ()
  (pmd//js2-setup)
  (cl-letf (((symbol-function 'file-name-base)
             (lambda () "myawesomefilename")))
    (should (equal  "console.log(\"myawesomefilename: \" + \"var1 = \" + var1 + \"  |  \" + \"var2 = \" + var2);"  (pmd//prepare-output (list "var1" "var2"))))
    (should (equal "console.log(\"myawesomefilename: \" + \"fn(\\\"var\\\") = \" + fn(\"var\"));" (pmd//prepare-output (list "fn(\"var\")"))))))

(ert-deftest pmd//rust-prepare-output-test ()
  (pmd//rust-setup)
  (should (equal "println!(\"var1 = {:?} | var2 = {:?}\", var1, var2);" (pmd//prepare-output (list "var1" "var2")))))

(ert-deftest pmd//print-vars-internal-test ()
  (with-temp-buffer
    (pmd//ruby-setup)
    (insert "1 + 1")
    (pmd//print-vars-internal "a,b")
    (pmd//print-vars-internal "el/(mapconcat 'identity '(\"var1\" \"var2\") \",\")")
    (pmd//print-vars-internal "sh/seq 1 1 3 | tr '\n' ',' | sed 's/,$//'")
    (pmd//print-vars-internal "rb/('a'..'d').to_a.join(',')")
    (cl-letf (((symbol-function 'file-name-base)
               (lambda () "file")))
      (pmd//print-vars-internal "fl/test"))
    (should (equal (buffer-string)
                   (mapconcat
                    'identity
                    (list  "1 + 1"
                           "puts \"a = #{a} | b = #{b}\""
                           "puts \"var1 = #{var1} | var2 = #{var2}\""
                           "puts \"1 = #{1} | 2 = #{2} | 3 = #{3}\""
                           "puts \"a = #{a} | b = #{b} | c = #{c} | d = #{d}\""
                           "puts \"file: test = #{test}\"")
                    "\n")))))
