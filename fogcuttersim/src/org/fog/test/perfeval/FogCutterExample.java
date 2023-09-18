package org.fog.test.perfeval;

import org.cloudbus.cloudsim.Log;
import org.cloudbus.cloudsim.core.CloudSim;
import org.fog.application.AppLoop;
import org.fog.application.Application;
import org.fog.application.selectivity.FractionalSelectivity;
import org.fog.placement.*;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;

import org.fog.entities.FogBroker;
import org.fog.entities.PhysicalTopology;
import org.fog.utils.JsonToTopology;

/**
 * Fog Cutter
 */
public class FogCutterExample {

    static String ApplicationPath = "fogCutter/Application.json";
    static String InfraStructurePath = "fogCutter/infrastructure.json";
    private static int Manual = 4; //1 - manual,  2 -Cloud,  3-Edge, 4- MobileEdgewards


    public static void main(String[] args) {

        Log.printLine("Starting  ...");

        try {
            Log.disable();
            int num_user = 1; // number of cloud users
            Calendar calendar = Calendar.getInstance();
            boolean trace_flag = false; // mean trace events

            CloudSim.init(num_user, calendar, trace_flag);

            String appId = "game";

            FogBroker broker = new FogBroker("broker");

            /*
             * Creating the application from specified JSON file
             */
            Application application = getApp(appId, broker.getId(), ApplicationPath);
            application.setUserId(broker.getId());

            /*
             * Creating the physical topology from specified JSON file
             */
            PhysicalTopology physicalTopology = JsonToTopology.getPhysicalTopology(broker.getId(), appId,
                    InfraStructurePath);


            Controller controller = new Controller("master-controller", physicalTopology.getFogDevices(),
                    physicalTopology.getSensors(),
                    physicalTopology.getActuators());

            if (Manual == 1) {                      //Arbitary
                ModuleMapping moduleMapping = ModuleMapping.createModuleMapping(); // initializing a module mapping
                moduleMapping.addModuleToDevice("client", "cloud"); // fixing instances of User Interface module in the Cloud

                // if the mode of deployment is cloud-based
                moduleMapping.addModuleToDevice("classifier", "cloud"); // placing all instances of Object Detector module in the Cloud
                moduleMapping.addModuleToDevice("tuner", "cloud"); // placing all instances of Object Tracker module in the Cloud
                controller.submitApplication(application, (new ModulePlacementMapping(
                                physicalTopology.getFogDevices(),
                                application,
                                moduleMapping
                        ))
                );
            } else if (Manual == 2) { //Cloud
                controller.submitApplication(application, (new ModulePlacementOnlyCloud(
                                physicalTopology.getFogDevices(),
                                physicalTopology.getSensors(), physicalTopology.getActuators(),
                                application
                        ))
                );
            } else if (Manual==3) {    //Edgeward

                controller.submitApplication(application, 0,
                        new ModulePlacementEdgewards(physicalTopology.getFogDevices(),
                                physicalTopology.getSensors(), physicalTopology.getActuators(),
                                application, ModuleMapping.createModuleMapping()));

            }
            else if (Manual==4){  //Mobile Edgewards
                controller.submitApplication(application, 0,
                        new ModulePlacementMobileEdgewards(physicalTopology.getFogDevices(),
                                physicalTopology.getSensors(), physicalTopology.getActuators(),
                                application, ModuleMapping.createModuleMapping()));
            }
            else{

            }

            CloudSim.startSimulation();

            CloudSim.stopSimulation();

            Log.printLine(" finished!");
        } catch (
                Exception e) {
            e.printStackTrace();
            Log.printLine("Unwanted errors happen");
        }

    }

    public static Application getApp(String appId, int userId, String physicalTopologyFile) throws Exception {
        Application application = Application.createApplication(appId, userId);

        try {
            JSONObject doc = (JSONObject) JSONValue.parse(new FileReader(physicalTopologyFile));
            JSONArray nodes = (JSONArray) doc.get("AppModule");
            @SuppressWarnings("unchecked")
            Iterator<JSONObject> iter = nodes.iterator();
            while (iter.hasNext()) {
                JSONObject node = iter.next();
                Long ram = (Long) node.get("ram");
                String nodeName = (String) node.get("name");
                application.addAppModule(nodeName, ram.intValue());
            }

            JSONArray TupleMapping = (JSONArray) doc.get("TupleMapping");
            @SuppressWarnings("unchecked")
            Iterator<JSONObject> TupleMappingiter = TupleMapping.iterator();
            while (TupleMappingiter.hasNext()) {
                JSONObject node = TupleMappingiter.next();

                String nodeName = (String) node.get("name");
                String inputTupleType = (String) node.get("inputTupleType");
                String outputTupleType = (String) node.get("outputTupleType");
                double FractionalSelectivity = (double) node.get("FractionalSelectivity");
                application.addTupleMapping(nodeName, inputTupleType, outputTupleType, new FractionalSelectivity(FractionalSelectivity));
            }

            JSONArray links = (JSONArray) doc.get("AppEdge");
            @SuppressWarnings("unchecked")
            Iterator<JSONObject> linksIter = links.iterator();
            while (linksIter.hasNext()) {
                JSONObject link = linksIter.next();
                String src = (String) link.get("source");
                String dst = (String) link.get("destination");
                Long tupleCpuLength = (Long) link.get("tupleCpuLength");
                Long tupleNwLength = (Long) link.get("tupleNwLength");
                String tupleType = (String) link.get("tupleType");
                Long direction = (Long) link.get("direction"); //Tuple.UP
                Long edgeType = (Long) link.get("edgeType"); //AppEdge.SENSOR

                application.addAppEdge(src, dst, tupleCpuLength.intValue(), tupleNwLength.intValue(), tupleType, direction.intValue(), edgeType.intValue());

            }
            List<AppLoop> loops = new ArrayList<AppLoop>();

            JSONArray loop = (JSONArray) doc.get("loop");
            @SuppressWarnings("unchecked")
            Iterator<JSONObject> loopIter = loop.iterator();
            while (loopIter.hasNext()) {
                JSONObject loops1 = loopIter.next();
                ArrayList<String> name = new ArrayList<String>();

                for (int i = 1; i <= loops1.size(); i++) {
                    name.add(loops1.get(i + "").toString());
                }
                final AppLoop loop1 = new AppLoop(new ArrayList<String>() {{
                    for (int i = 0; i < name.size(); i++) {
                        add(name.get(i));
                    }
                }});
                loops.add(loop1);
            }
            application.setLoops(loops);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return application;
    }


   /* @SuppressWarnings({"serial"})
    private static Application createApplication(String appId, int userId) {

        MicroserviceAppSample2 -> Loading of App from File

     }*/


}