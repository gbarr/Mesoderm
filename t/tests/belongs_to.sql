use testdb;

create table t1 (
  id int primary key auto_increment,
  name varchar(40),
  unique key `name` (name)
) engine=InnoDB;

create table t2 (
  id int primary key auto_increment,
  t int,
  constraint `t` foreign key (`t`) references t1 (`id`) on delete cascade
) engine=InnoDB;

