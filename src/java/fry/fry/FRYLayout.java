package fry;

import java.lang.reflect.Field;
import java.util.Hashtable;

public class FRYLayout{

	public Hashtable<Integer,String> layout; // Maps the value names to their position number
	public Hashtable<Integer, String> datatype; // Maps the position value to their datatype 
	public int numFields;

	public FRYLayout(){
		buildHashtable();
	}
	
	/**
	 * Creates an anonymous layout (no field names)
	 * @param numFields
	 */
	public FRYLayout(int numFields){
		layout = new Hashtable<Integer, String>();
		datatype = new Hashtable<Integer, String>();
		for (int i = 0; i < numFields; i++){
			layout.put(i, (new Integer(i)).toString());
		}
	}
	
	public FRYLayout(Object[] record){
		
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
			datatype.put(i, "String");
			i++;
		}
		numFields=i;
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
	
	public Field getField(int index) throws NoSuchFieldException, SecurityException{
		return this.getClass().getDeclaredField(layout.get(index));
	}
	
	public Integer getIdByName(String fieldName){
		/**
		 * This is inefficient but don't have enough time to fix things
		 */
		for(int i = 0; i < layout.size(); i++){
			if(layout.get(i).equals(fieldName)){
				return i;
			}
		}
		return null;
	}

}