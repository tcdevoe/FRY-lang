import fry.*;import java.util.Arrays;
import java.io.IOException;
import java.util.ArrayList;
public class fry{
public static class weblog extends FRYLayout{
 public String date;
public String site;
public String user;
public weblog(String date,String site,String user){

 super();
this.user=user;
this.site=site;
this.date=date;}
public weblog(){
super();}
public String toString(){
return date.toString()+"|"+site.toString()+"|"+user.toString();
}}

public static void main(String[] args) throws IOException{
;
FRYTable weblog_data = new FRYTable( new weblog() );
weblog_data.readInput(IOUtils.Read("website_hits.txt", "|"));
FRYTable frodo_facebook;{FRYList<String[]>  __ret_data__ = new FRYList<String[]>(weblog_data.getData().size());
for(String[] i : weblog_data.getData()){

if((((i[weblog_data.layout.getIdByName("site")]))).equals("facebook.com") &&(((i[weblog_data.layout.getIdByName("user")]))).equals("Frodo") ){ __ret_data__.add(i);}
}FRYTable __tmp_tbl__  = new FRYTable(__ret_data__,weblog_data.layout);
frodo_facebook = __tmp_tbl__;}
FRYTable sam_google;{FRYList<String[]>  __ret_data__ = new FRYList<String[]>(weblog_data.getData().size());
for(String[] i : weblog_data.getData()){

if((((i[weblog_data.layout.getIdByName("site")]))).equals("google.com") &&(((i[weblog_data.layout.getIdByName("user")]))).equals("Sam") ){ __ret_data__.add(i);}
}FRYTable __tmp_tbl__  = new FRYTable(__ret_data__,weblog_data.layout);
sam_google = __tmp_tbl__;}
IOUtils.Write("frodo_facebook.txt", frodo_facebook);
IOUtils.Write("sam_google.txt", sam_google);
}
}