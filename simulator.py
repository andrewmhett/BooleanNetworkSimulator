import sys
import json

if len(sys.argv[1:])>1:
    print("Only one argument accepted as SAVE_PATH.")
    exit(-1)

import drawing
import asyncio

if len(sys.argv[1:])==1:
    drawing.save_path=sys.argv[1]

def button_simulate(self):
    if self.pressed:
        self.outputs[0].state=self.inputs[0].state
    else:
        self.outputs[0].state=False

def and_simulate(self):
    self.outputs[0].state = self.inputs[0].state and self.inputs[1].state

def high_bit_simulate(self):
    self.outputs[0].state=True

def or_simulate(self):
    self.outputs[0].state = self.inputs[0].state or self.inputs[1].state

def nor_simulate(self):
    self.outputs[0].state = not (self.inputs[0].state or self.inputs[1].state)

def not_simulate(self):
    self.outputs[0].state = not self.inputs[0].state

def nand_simulate(self):
    self.outputs[0].state = not (self.inputs[0].state and self.inputs[1].state)

drawing.BUTTON.simulate=button_simulate
drawing.AND.simulate=and_simulate
drawing.HIGH_BIT.simulate=high_bit_simulate
drawing.OR.simulate=or_simulate
drawing.NOR.simulate=nor_simulate
drawing.NAND.simulate=nand_simulate
drawing.NOT.simulate=not_simulate

def save(filename):
    if "." in filename:
        filename=filename.split(".")[0]
    filename+=".txt"
    with open("saves/{0}".format(filename),"w+") as f:
        json.dump({"scale_factor":drawing.scale_factor,"x_offset":drawing.x_offset,"y_offset":drawing.y_offset},f)
        f.write("\n")
        for component in drawing.components:
            json.dump(component.dump(),f)
            f.write("\n")
        for conn_point in drawing.conn_points:
            json.dump(conn_point.dump(),f)
            f.write("\n")
        for connection in drawing.connections:
            json.dump(connection.dump(),f)
            f.write("\n")
        f.close()

def load(filename):
    if "." in filename:
        filename=filename.split(".")[0]
    filename+=".txt"
    components=[]
    conn_points=[]
    connections=[]
    with open("saves/{0}".format(filename),"r") as f:
        line_counter=0
        for line in f:
            line_counter+=1
            if line_counter==1:
                view_data=json.loads(line)
                drawing.scale_factor=view_data['scale_factor']
                drawing.x_offset=view_data['x_offset']
                drawing.y_offset=view_data['y_offset']
            else:
                j=json.loads(line)
                if j['category']=="COMPONENT":
                    components.append(j)
                if j['category']=="CONNECTION_POINT":
                    conn_points.append(j)
                if j['category']=="CONNECTION":
                    connections.append(j)
        f.close()
    typeStringLookup={
        "AND":drawing.AND,
        "OR":drawing.OR,
        "NOT":drawing.NOT,
        "NOR":drawing.NOR,
        "NAND":drawing.NAND,
        "BUTTON":drawing.BUTTON,
        "HIGH_BIT":drawing.HIGH_BIT
    }
    input_counter={}
    output_counter={}
    for component in components:
        typeStringLookup[component['type']](component['x'],component['y'],id=component['id'])
        input_counter[component['id']]=0
        output_counter[component['id']]=0
    for component in drawing.components:
        for conn_point in conn_points:
            if component.id==conn_point['parent_id']:
                if conn_point['io']=="INPUT":
                    component.inputs[input_counter[component.id]].id=conn_point['id']
                    input_counter[component.id]+=1
                if conn_point['io']=="OUTPUT":
                    component.outputs[output_counter[component.id]].id=conn_point['id']
                    output_counter[component.id]+=1
    for connection in connections:
        for conn_point in drawing.conn_points:
            if conn_point.id == connection['p1_id']:
                p1=conn_point
            if conn_point.id == connection['p2_id']:
                p2=conn_point 
        drawing.Connection(p1,p2,id=connection['id'])

if drawing.save_path != None:
    load(drawing.save_path)

drawing.load_callback=load
drawing.save_callback=save

async def simulate_loop():
    while True:
        for connection in drawing.connections:
            in_connector=connection.p1 if (connection.p1.io==drawing.IO.INPUT) else connection.p2
            out_connector=connection.p1 if (connection.p1.io==drawing.IO.OUTPUT) else connection.p2
            if out_connector.state:
                connection.state=True
                connection.p1.state=True
                connection.p2.state=True
            else:
                on=False
                for conn_2 in drawing.connections:
                    if conn_2 != connection:
                        if conn_2.p1 == in_connector or conn_2.p2 == in_connector:
                            if conn_2.state == True:
                                on=True
                connection.state=False
                out_connector=False
                in_connector.state=on
        for component in drawing.components:
            component.simulate()
        await asyncio.sleep(1/drawing.framerate)

asyncio.get_event_loop().create_task(simulate_loop())
asyncio.get_event_loop().create_task(drawing.update_loop())
asyncio.get_event_loop().create_task(drawing.draw_loop())
asyncio.get_event_loop().run_forever()
