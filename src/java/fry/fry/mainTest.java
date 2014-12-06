package fry; 

import java.io.IOException;

class date extends FRYLayout{
	Integer day;
	Integer mon;
	Integer year;
	
	public date(Integer day, Integer mon, Integer year){
		this.day = day;
		this.mon = mon;
		this.year = year;
	}
}

public class mainTest {	
	
	public static void main(String[] args) throws IOException{
		FRYTable tab = IOUtils.Read("C:\\Users\\tdevoe\\Documents\\Columbia\\FRY-lang\\src\\java\\test.in", ",");
		IOUtils.Write("C:\\Users\\tdevoe\\Documents\\Columbia\\FRY-lang\\src\\java\\out1.txt", tab, "&");
		FRYLayout rec = new date(5,6,7);
		tab.append(rec);
		tab.resetIndex();
		IOUtils.Write("C:\\Users\\tdevoe\\Documents\\Columbia\\FRY-lang\\src\\java\\out2.txt", tab, "&");

	}
	
}
