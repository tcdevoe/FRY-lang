package fry;

import java.io.BufferedReader;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.regex.Pattern;

public class FRYTable {
	public FRYLayout layout; // Maps the value names to their position number
	public boolean isLayoutSet;
	private int numFields; //Number of fields in a record
	private ArrayList<String[]> data; //data stored in table
	int recIndex, numRecords;
	
	public FRYTable(){
		layout = new FRYLayout();
		isLayoutSet = false;
		recIndex = 0;
		data = new ArrayList<String[]>();
	}
	
	 public ArrayList<String[]> getData() {
		return data;
	}
	 
	public FRYTable(ArrayList<String[]> data, FRYLayout layout){
		this(layout);
		this.data = data;
		numRecords = data.size();
	}
	
	public FRYTable(FRYLayout layout){
		this.layout = layout;
		isLayoutSet = true;
		recIndex = 0;
		data = new ArrayList<String[]>();
	}

	public void readInput(InputFile i) throws IOException{
		String line = null;
		String[] record = null;
		BufferedReader buf = i.buf;
		String delim = i.delim;
		data = new ArrayList<String[]>();
		numRecords = 0;
		while ( (line = buf.readLine()) != null){
			record = parseRecord(line, delim);
			data.add(record);
			numRecords++;
		}
		numFields = record.length;
		recIndex = 0;
		if(layout == null){
			this.layout = new FRYLayout(numFields);
		}
	}
	
	public String[] parseRecord(String line, String delim){
		String[] record = line.split(Pattern.quote(delim));
		return record;
	}
	/**
	 * Returns next record in the dataset
	 * 
	 * @return
	 * Returns next record in the FRYTable if it exists, returns null if there are no more 
	 * records
	 */
	public String[] readRecord(){
		if(recIndex == numRecords){
			return null;
		}
		String[] dat = data.get(recIndex);
		recIndex++;
		return dat;
	}
	
	public FRYList<String> getColumn(int i){
		String[] col = new String[data.size()];
		for(int j = 0; j < data.get(0).length; j++){
			col[j] = data.get(j)[i];
		}
		return new FRYList<String>(Arrays.asList(col));
	}
	
	
	public FRYList<String> getColumn(String fieldName){
		return getColumn(layout.getIdByName(fieldName));
	}
	
	public void append(FRYLayout rec){
		Field[] fields = rec.getFields();
		String[] record = new String[fields.length];
		int i = 0;
		for (Field field : fields){
			try {
					if (field.get(rec) == null){
						record[i] = "";
					}
					else {
						record[i] = field.get(rec).toString();
					}
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