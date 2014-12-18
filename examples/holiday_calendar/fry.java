import fry.*;import java.util.Arrays;
import java.io.IOException;
import java.util.ArrayList;
public class fry{
public static class date extends FRYLayout{
 public String mon;
public Integer day;
public Integer year;
public String holiday;
public date(String mon,Integer day,Integer year,String holiday){

 super();
this.holiday=holiday;
this.year=year;
this.day=day;
this.mon=mon;}
public date(){
super();}
public String toString(){
return mon.toString()+"|"+day.toString()+"|"+year.toString()+"|"+holiday.toString();
}}public static Boolean isValidDate(String mon,Integer day) {
if ( day>28&&mon.equals("Feb") )
{
return false;
}

if ( day.equals(31) )
{
if ( mon.equals("Sep") ||mon.equals("Apr") ||mon.equals("Jun") ||mon.equals("Nov") )
{
return false;
}
else
{
return true;
}

}

return true;}

public static void main(String[] args) throws IOException{
;
FRYTable holidays = new FRYTable( new date() );
holidays.readInput(IOUtils.Read("holidays.txt", ","));
String holiday_name;
FRYTable calendar = new FRYTable( new date() );
FRYList<String> holiday_list;
FRYTable matching_days;
for (String mon: new FRYList<String>(Arrays.asList(new  String[]{("Jan"), ("Feb"), ("Mar"), ("Apr"), ("May"), ("Jun"), ("Jul"), ("Aug"), ("Sep"), ("Oct"), ("Nov"), ("Dec")}))) {
{
for (Integer day: FRYListFactory.getGeneratedFryList(1,31)) {
{
if ( isValidDate(mon, day))
{
{FRYList<String[]>  __ret_data__ = new FRYList<String[]>(holidays.getData().size());
for(String[] i : holidays.getData()){

if((((i[holidays.layout.getIdByName("mon")]))).equals(mon) &&(new Integer(Integer.parseInt(i[holidays.layout.getIdByName("day")]))).equals(day) ){ __ret_data__.add(i);}
}FRYTable __tmp_tbl__  = new FRYTable(__ret_data__,holidays.layout);;
matching_days = __tmp_tbl__;};
holiday_list = matching_days.getColumn("holiday");
holiday_name = holiday_list.get(0);
IOUtils.Append(calendar, new date (mon,day,2015,holiday_name));
}

}}
}}
IOUtils.Write("holiday-calendar.txt", calendar);
}
}