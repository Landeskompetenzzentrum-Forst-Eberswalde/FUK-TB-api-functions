import { time } from 'console';
import fetch from 'node-fetch';

const timeSeries = [];
let isSending = false;


const startTime = Date.now();

async function sendData(timeSeriesToSend) {
    
    if (timeSeriesToSend.length === 0 || isSending) {
        return;
    }
    isSending = true;
    const copyTimeSeries = [...timeSeriesToSend];
    timeSeries.length = 0;

    const url = `https://thingsboard.gruenecho.de:443/api/v1/${process.env.DEVICE_ACCESS_TOKEN}/telemetry`;

    try {
        console.log('Sending data to', copyTimeSeries);
        const response = await fetch(url, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(copyTimeSeries),
        });
        console.log('Response:', response.status);
        isSending = false;
    } catch (error) {
        console.error('Error:', error);
        timeSeries.push(...copyTimeSeries);
        isSending = false;
    }
}

async function sinus() {
    const created = Date.now();
    let timestamp = (created / 1000);
    timestamp = timestamp/60;

  
    timeSeries.push({
        ts: Math.round(created),
        values: {
            sin: Math.sin(timestamp)
        }
    });
    console.log(timeSeries.length);
    return timeSeries;
}

async function start() {
    setInterval(async () => {
        await sinus();
        sendData(timeSeries);
    }, 1000);
}

start();