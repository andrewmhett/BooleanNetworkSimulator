import drawing
import asyncio

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
        await asyncio.sleep(3/drawing.framerate)

asyncio.get_event_loop().create_task(simulate_loop())
asyncio.get_event_loop().create_task(drawing.update_loop())
asyncio.get_event_loop().create_task(drawing.draw_loop())
asyncio.get_event_loop().run_forever()
