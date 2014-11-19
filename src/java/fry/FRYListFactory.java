package fry;

import java.util.ArrayList;
import java.util.Arrays;

public class FRYListFactory{

	public static ArrayList<Integer> getGeneratedFryList(int min, int max){
		ArrayList<Integer> list = new ArrayList<Integer>(max-min);
		for(int i = min; i < max; i++){
			list.add(new Integer(i));
		}
		return list;
	}
}