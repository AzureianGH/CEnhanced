const canvas = document.getElementById("simCanvas");
const ctx = canvas.getContext("2d");

canvas.width = window.innerWidth * 0.8;
canvas.height = window.innerHeight * 0.6;

const gravity = 0.5;
const friction = 0.9;
const balls = [];

class Ball {
    constructor(x, y, radius, color) {
        this.x = x;
        this.y = y;
        this.radius = radius;
        this.color = color;
        this.vx = (Math.random() - 0.5) * 8;
        this.vy = (Math.random() - 0.5) * 8;
        this.mass = radius; // mass proportional to radius
    }

    draw() {
        ctx.beginPath();
        ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
        ctx.fillStyle = this.color;
        ctx.fill();
        ctx.closePath();
    }

    update() {
        this.vy += gravity;

        this.x += this.vx;
        this.y += this.vy;

        // Walls collision
        if (this.x - this.radius < 0) {
            this.x = this.radius;
            this.vx *= -friction;
        }
        if (this.x + this.radius > canvas.width) {
            this.x = canvas.width - this.radius;
            this.vx *= -friction;
        }
        if (this.y - this.radius < 0) {
            this.y = this.radius;
            this.vy *= -friction;
        }
        if (this.y + this.radius > canvas.height) {
            this.y = canvas.height - this.radius;
            this.vy *= -friction;
        }

        this.draw();
    }
}

// Elastic collision resolution
function resolveCollision(b1, b2) {
    const dx = b2.x - b1.x;
    const dy = b2.y - b1.y;
    const distance = Math.hypot(dx, dy);

    if (distance === 0) return;

    if (distance < b1.radius + b2.radius) {
        const nx = dx / distance;
        const ny = dy / distance;

        const vxRel = b2.vx - b1.vx;
        const vyRel = b2.vy - b1.vy;

        const velAlongNormal = vxRel * nx + vyRel * ny;
        if (velAlongNormal > 0) return;

        const restitution = 0.9;
        const impulse = (-(1 + restitution) * velAlongNormal) / (1 / b1.mass + 1 / b2.mass);

        const ix = impulse * nx;
        const iy = impulse * ny;

        b1.vx -= ix / b1.mass;
        b1.vy -= iy / b1.mass;
        b2.vx += ix / b2.mass;
        b2.vy += iy / b2.mass;

        // Separate balls proportionally to mass
        const overlap = b1.radius + b2.radius - distance;
        const totalMass = b1.mass + b2.mass;
        b1.x -= (overlap * (b2.mass / totalMass)) * nx;
        b1.y -= (overlap * (b2.mass / totalMass)) * ny;
        b2.x += (overlap * (b1.mass / totalMass)) * nx;
        b2.y += (overlap * (b1.mass / totalMass)) * ny;
    }
}

// Add balls on click
canvas.addEventListener("click", (e) => {
    const radius = Math.random() * 20 + 15;
    const color = `hsl(${Math.random() * 360}, 60%, 50%)`;
    balls.push(new Ball(e.offsetX, e.offsetY, radius, color));
});

function animate() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);

    // Update all balls and resolve collisions
    for (let i = 0; i < balls.length; i++) {
        balls[i].update();
        for (let j = i + 1; j < balls.length; j++) {
            resolveCollision(balls[i], balls[j]);
        }
    }

    requestAnimationFrame(animate);
}

animate();