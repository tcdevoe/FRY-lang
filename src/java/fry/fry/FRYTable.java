package fry;

import java.io.BufferedReader;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;

public class FRYTable {
	public FRYLayout layout; // Maps the value names to their position number
	public boolean isLayoutSet;
	private int numFields; //Number of fields in a record
	private ArrayList<Object[]> data; //data stored in table
	int recIndex, numRecords;
	
	public FRYTable(){
		layout = new FRYLayout();
		isLayoutSet = false;
		recIndex = 0;
	}
	
	public FRYTable(FRYLayout layout){
		this.layout = layout;
		isLayoutSet = true;
		recIndex = 0;
	}

	public FRYTable(BufferedReader buf, String delim) throws IOException{
		String line = null;
		Object[] record = null;
		data = new ArrayList<Object[]>();
		numRecords = 0;
		while ( (line = buf.readLine()) != null){
			record = parseRecord(line, delim);
			data.add(record);
			numRecords++;
		}
		numFields = record.length;
		recIndex = 0;
		layout = new FRYLayout();
	}
	
	public Object[] parseRecord(String line, String delim){
		String[] record = line.split(delim);
		return record;
	}
	/**
	 * Returns next record in the dataset
	 * 
	 * @return
	 * Returns next record in the FRYTable if it exists, returns null if there are no more 
	 * records
	 */
	public Object[] readRecord(){
		if(recIndex == numRecords){
			return null;
		}
		Object[] dat = data.get(recIndex);
		recIndex++;
		return dat;
	}
	
	public ArrayList<Object> getColumn(int i){
		Object[] col = new Object[data.size()];
		for(int j = 0; j < data.size(); j++){
			col[j] = data.get(j)[i];
		}
		return new ArrayList<Object>(Arrays.asList(col));
	}
	
	public ArrayList<Object> getColumn(String fieldName){
		return getColumn(layout.getIdByName(fieldName));
	}
	
	public void append(FRYLayout rec){
		Field[] fields = rec.getFields();
		Object[] record = new Object[fields.length];
		int i = 0;
		for (Field field : fields){
			try {
				record[i] = field.get(rec);
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			i++;
		}
		data.add(record);
		numRecords++;
	}
	
	public void resetIndex(){
		recIndex = 0;
	}

}