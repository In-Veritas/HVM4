wnf_eql_emp_emp :: WnfEql
wnf_eql_emp_emp e s Emp Emp = do
  inc_inters e
  wnf e s Tru

