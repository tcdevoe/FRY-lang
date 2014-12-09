package fry;

import java.util.ArrayList;
import java.util.Arrays;
import java.io.IOException;
public class mainTest{
private static class username extends FRYLayout{
String fname;
String lname;
public username(String fname,String lname){

 super();
this.lname=lname;
this.fname=fname;}
public username(){
super();}
public String toString(){
return fname.toString()+"|"+lname.toString();
}}
private static class user extends FRYLayout{
Integer id;
String fname;
String lname;
public user(Integer id,String fname,String lname){

 super();
this.lname=lname;
this.fname=fname;
this.id=id;}
public user(){
super();}
public String toString(){
return id.toString()+"|"+fname.toString()+"|"+lname.toString();
}}

public static void main(String[] args) throws IOException{
;
;
FRYTable user_tbl = new FRYTable( new user() );
user_tbl.readInput(IOUtils.Read("table_test02.in", "|"));
ArrayList<String[]>  __ret_data__ = new ArrayList<String[]>(user_tbl.getData().size());
for(String[] i : user_tbl.getData()){
int __index_id__ = user_tbl.layout.getIdByName("id");


if(Integer.parseInt(i[user_tbl.layout.getIdByName("id")])>4){ 
	__ret_data__.add(new String[]{(i[user_tbl.layout.getIdByName("fname")]),(i[user_tbl.layout.getIdByName("lname")])});
}
}FRYTable __tmp_tbl__  = new FRYTable(__ret_data__,user_tbl.layout);
FRYTable test = __tmp_tbl__;
IOUtils.Write(IOUtils.stdout, test);
}
}

