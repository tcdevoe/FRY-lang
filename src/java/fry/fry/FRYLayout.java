package fry;

import java.lang.reflect.Field;
import java.util.Hashtable;

public class FRYLayout{

	public Hashtable<Integer,String> layout; // Maps the value names to their position number
	public Hashtable<Integer, String> datatype; // Maps the position value to their datatype 

	public FRYLayout(){
		buildHashtable();
	}
	
	public void buildHashtable(){
		layout = new Hashtable<Integer, String>();
		datatype = new Hashtable<Integer, String>();
		int i = 0;
		for (Field field : this.getClass().getDeclaredFields()){
			if ( field.getName().equals("layout") || 
					field.getName().equals("datatype")){
				continue;
			}
			layout.put(i, field.getName());
			datatype.put(i, "Integer");
			i++;
		}
	}
	
	public String toString(){
		String out = "";
		for (Field field : this.getClass().getDeclaredFields()){
			out += field.toString()+"|";
		}
		return out;
	}
	
	public Field[] getFields(){
		Field[] fields = new Field[this.getClass().getDeclaredFields().length];
		int i = 0;
		for (Field field : this.getClass().getDeclaredFields()){
			if ( field.getName().equals("layout") || 
					field.getName().equals("datatype")){
				continue;
			}
			fields[i] = field;
			i++;
		}
		return fields;
	}
	
	public String getIdByName(String fieldName){
		return layout.get(fieldName);
	}

}