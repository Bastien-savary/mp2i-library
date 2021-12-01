/* [sum]returns the sum of elements of t[] */
int sum (int* t, int n){
	int plus = 0;
	for (int i = 0; i < n; i++){
		plus += t[i];
	}
return plus;
}

/*[mem] returns if e belongs to t[]*/
bool mem (int*t, int n, int e){
	bool test = false;
	for(int i = 0; i<n; i++){
		if(t[i]==e){
			test = true;
			break;
		}
	}
	return test;
} 

/*[minimum] returns the minimum of t[]*/
int minimum (int* t, int n){
	int min = INT_MAX;
	for (int i=0; i<n; i++){
		if(t[i] < min){
			min = t[i];
		} 
	} 
	return min;
}

/*[maximum] returns the maximum of t[]*/
int maximum (int* t, int n){
	int max = INT_MIN;
	for (int i=0; i<n; i++){
		if(t[i] > min){
			max = t[i];
		} 
	} 
	return max;
}

/*[increase] returns if t[] increases*/
bool increase (int* t, int n){
	bool test = true;  
	for (int i = 0; i < n-1; i++){
		if(t[i]>t[i+1]){
			test = false 
		}

	}
	return test;
}

/*[decrease] returns if t[] increases*/
bool decrease (int* t, int n){
	bool test = true;  
	for (int i = 0; i < n-1; i++){
		if(t[i]<t[i+1]){
			test = false 
		}

	}
	return test;
}

/*[doublon] returns if t[] have one duplicate*/
bool doublon (int* t, int n){
	bool test = false;
	for(int i = 0; i<n-1 ; i++){
		for (int j = i+1; j<n ; j++){
			if (t[i]==t[j]){
				test = true;
			}
		}
	}
	return test;
}

/*[equal] returns true if the two array have the same lentgh and elements*/
bool equal (int* t1, int n1, int* t2, int n2){
	bool result = true;
	if (n1 != n2){
		result = false;
	}
	else {
		for (int i = 0; i<n1 ; i++){
			if(t1[i] != t2[i]){
				result = false;
			}
		}
	}
	return result;
}

/*[consecutive] returns the higher sum in t[] */
int consecutive_max (int* t, int n){
	int m = t[0];
	int m_cur = t[0];
	for (int i = 0; i<n; i++){
		m_cur = max(m_cur + t[i], t[i]);
		m = max (m, m_cur);
	}
	return m;
}

/*[print] to print the array*/
void print (int* t, int n){
	for (int i = 0; i <= n; i++){
		printf( "%d ", t[i]);
	}
}

/*[lentgh] returns the number of int in t[]*/
void length (int tab){
	size_t  n = sizeof(tab)/sizeof(int);
    printf("%d\n",n)
}

/*[dichotomie] returns if e belongs to t[] in O(nlog(n))*/
bool dichotomie (int* t, int e, int i, int j){
	bool test = false;
	int m = (i+j)/2;
	if (i > j){
		return test; 
	}
	else if(t[m] < e){
		return dichotomie(t, e,m+1,j);
	}
	else if (t[m] > e){
		return dichotomie(t,e,i,m-1);
	}
	else{
		test = true;
	}
	return test;
}
