#!/usr/bin/env bash

cat <<EOT >> src/main.cc

#include <iostream>
#include <vector>

using namespace std;


int
main (int argc, char *argv[]) {

	cout << "$1 v0.1" << endl;

	return(0);
}

EOT


