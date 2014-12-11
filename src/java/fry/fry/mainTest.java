package fry;

import java.util.ArrayList;
import java.util.Arrays;
import java.io.IOException;
public class mainTest{
private static class in_layout extends FRYLayout{
Integer id;
String fname;
String lname;
String city;
String state;
public in_layout(Integer id,String fname,String lname,String city,String state){

 super();
this.state=state;
this.city=city;
this.lname=lname;
this.fname=fname;
this.id=id;}
public in_layout(){
super();}
public String toString(){
return id.toString()+"|"+fname.toString()+"|"+lname.toString()+"|"+city.toString()+"|"+state.toString();
}}

public static void main(String[] args) throws IOException{
;
FRYTable tbl = new FRYTable( new in_layout() );
tbl.readInput(IOUtils.Read("C://Users//tdevoe//Documents//Columbia//FRY-lang//examples//filter//in.txt", ","));
ArrayList<String[]>  __ret_data__ = new ArrayList<String[]>(tbl.getData().size());
for(String[] i : tbl.getData()){
int __index_state__ = tbl.layout.getIdByName("state");


if(!(i[tbl.layout.getIdByName("state")]).equals("NJ")){ __ret_data__.add(i);}
}FRYTable __tmp_tbl__  = new FRYTable(__ret_data__,tbl.layout);
FRYTable out = __tmp_tbl__;
IOUtils.Write("C://Users//tdevoe//Documents//Columbia//FRY-lang//examples//filter//out.txt", out);
}
}