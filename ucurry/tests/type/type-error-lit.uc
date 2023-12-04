check_type_error bool a = 1;
check_type_error bool b = "123";
check_type_error bool c = (1,2,3).1;
check_type_error int d = (1, 2, 3).9;
 