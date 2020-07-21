import drawing
import asyncio

asyncio.get_event_loop().create_task(drawing.update_loop())
asyncio.get_event_loop().run_forever()
