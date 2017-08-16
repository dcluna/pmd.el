(ert-deftest pmd//parse-input-test ()
  (should (equal (list "var1" "var2") (pmd//parse-input "var1\\,   var2")))
  (should (equal (list "[1,2,3]") (pmd//parse-input "[1,2,3]")))
  )

(ert-deftest pmd//ruby-prepare-output-test ()
  (pmd//ruby-setup)
  (should (equal "puts \"my-var = #{my-var}\"" (pmd//prepare-output (list "my-var"))))
  (should (equal "puts \"var1 = #{var1} | var2 = #{var2}\"" (pmd//prepare-output (list "var1" "var2"))))
  )

(ert-deftest pmd//js-prepare-output-test ()
  (pmd//js2-setup)
  (should (equal "console.log(\"var1 = \" + var1 + \"var2 = \" + var2)" (pmd//prepare-output (list "var1" "var2"))))
  )

(ert-deftest pmd//rust-prepare-output-test ()
  (pmd//rust-setup)
  (should (equal "println!(\"var1 = {:?} | var2 = {:?}\", var1, var2);" (pmd//prepare-output (list "var1" "var2"))))
  )

(ert-deftest pmd/print-vars-test ()
  (with-temp-buffer
    (pmd//ruby-setup)
    (insert "1 + 1")
    (pmd/print-vars "a,b")
    (should (equal (buffer-string) "1 + 1\nputs \"a = #{a} | b = #{b}\"")))
  )
