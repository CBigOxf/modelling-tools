MODEL=${1:-"name-of-your-file.nlogo3d"} #must be in Experiments folder, defaults to a specific file
ELIST=( $(grep -o -P '(?<=experiment name=")(.*)(?=" re)' ../$MODEL) )
function expSetup {
	
	PS3=$'\e[0mPlease enter your choice: '
	select EXP in "${ELIST[@]}"
	do
	 	echo "You chose experiment $EXP"
		read -e -p $'\e[0mEnter the name of the csv file in which the output will be saved: \e[35;1m' -i $EXP NAME
		read -e -p $'\e[0mEnter the path for the output file: \e[35;1m' -i "/home/your-path/netlogo-5.3.1-64/Experiments/egfr_het" DIR #defaults to a specific directory CHANGE THIS TO MATCH YOUR PATH
		read -e -p $'\e[0mEnter the number of cores to be used: \e[35;1m' -i "10" CORES
		echo -e "\e[0m\nConfirm your selection:\e[35;1m\nExperiment: $EXP\nOutput: $DIR/$NAME.csv\nCores: $CORES"
		read -e -p $'\e[0mConfirm (y/n)? \e[35;1m' -i "y" yn
		echo -e "\e[0m\n"
		case $yn in
			[Yy]* ) #CHANGE NEXT LINES TO MATCH YOUR PATH
				cmd=$"java -Dorg.nlogo.is3d=true  -Xmx16384m -Dfile.encoding=UTF-8 -cp /home/your-path/netlogo-5.3.1-64/app/NetLogo.jar 
					org.nlogo.headless.Main
					--model /home/your-path/netlogo-5.3.1-64/Experiments/$MODEL
					--experiment $EXP
					--spreadsheet $DIR/$NAME.csv
					--threads $CORES"
					eval $cmd;;
			[Nn]* ) expSetup;;
			* ) echo "Please answer yes or no."
				expSetup;;
		esac
		break
	done
}
echo -e "\e[35;1m"
expSetup