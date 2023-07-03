import json

f = open('example.json')
output = open('infrastructure.pl', "w")

data = json.load(f)

for i in data['nodes']:
	if i['type'] == 'ACTUATOR' or i['type'] == 'SENSOR':
		output.write('node(' + i['name'] + ').\n')
		output.write('nodeCap(' + i['name'] + ', type, ' + i['type'].lower() + ').\n')
		output.write('nodeCap(' + i['name'] + ', mips, 0).\n')
		output.write('nodeCap(' + i['name'] + ', ram, 0).\n')
		output.write('nodeCap(' + i['name'] + ', storage, 0).\n')
		output.write('nodeCap(' + i['name'] + ', downBw, 1000).\n')
		output.write('nodeCap(' + i['name'] + ', upBw, 1000).\n')
		output.write('\n')
	elif i['type'] == 'FOG_DEVICE':
		output.write('node(' + i['name'] + ').\n') # node(Name)
		output.write('nodeCap(' + i['name'] + ', type, ' + i['type'].lower() + ').\n')
		output.write('nodeCap(' + i['name'] + ', mips, ' + str(i['mips']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', ram, ' + str(i['ram']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', ratePerMips, ' + str(i['ratePerMips']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', upBw, ' + str(i['upBw']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', downBw, ' + str(i['downBw']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', level, ' + str(i['level']) + ').\n')
		output.write('\n')
	elif i['type'] == 'host':
		output.write('node(' + i['name'] + ').\n') # node(Name)
		output.write('nodeCap(' + i['name'] + ', type, ' + i['type'].lower() + ').\n')
		output.write('nodeCap(' + i['name'] + ', mips, ' + str(i['mips']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', ram, ' + str(i['ram']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', storage, ' + str(i['storage']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', bw, ' + str(i['upBw']) + ').\n')
		output.write('nodeCap(' + i['name'] + ', pes, ' + i['pes'] + ').\n')
		output.write('nodeCap(' + i['name'] + ', nums, ' + str(i['nums']) + ').\n')
		output.write('\n')
        

for i in data['links']:
    output.write('linkCap(' + i['source'] + ', ' + i['destination'] + ', latency, ' + str(i['latency']) + ').\n')
    output.write('linkCap(' + i['destination'] + ', ' + i['source'] + ', latency, ' + str(i['latency']) + ').\n')

# Closing file
f.close()
output.close()
